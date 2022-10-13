module Packrat

using MacroTools
# using MacroTools: @capture, unblock

export Node, @PEG, parse

abstract type Parslet end

struct Rule 
    name::Symbol
    node::Parslet
end

struct STR <: Parslet
    pattern::String
end

struct RGX <: Parslet
    pattern::Regex
end

struct SYM <: Parslet
    rulename::Symbol
end

struct SEQ <: Parslet
    children::Vector{Parslet}
end

struct ALT <: Parslet
    children::Vector{Parslet}
end

struct REP <: Parslet
    
end

function parse end

macro PEG(expr)
    @capture(expr, begin
        rules__
    end) || @error "syntax error"

    rules = map(rules) do rule
        @capture(rule, name_Symbol = node_) || @error "syntax error" 

        return Rule(name, _parse_node(node))
    end

    rules = map(_compile_rule, rules)

    quote
        function parse(str::String)
            res = $(first(rules))(str, 1)

            isnothing(res) ? nothing : first(res)
        end
    end |> unblock
end

##

function _parse_node(@nospecialize(node))

    @capture(node, str_String) && return STR(str)

    @capture(node, sym_Symbol) && return SYM(sym)

    if @capture(node, @r_str rx_)
        # node.args[1] == Symbol("@r_str")
        # node.args[2] == LineNumberNode(...)
        node.args[3] = '^' * rx

        return RGX(macroexpand(@__MODULE__, node))
    end

    @capture(node, _[*]) && @error "unimplemented"

    @capture(node, _[+]) && @error "unimplemented"

    @capture(node, op1_ | op2_) && return ALT(_parse_node.([op1, op2]))

    @capture(node, op1_ & op2_) && return SEQ(_parse_node.([op1, op2]))
    
    @error "syntax error: couldn't parse rule $(node)"

end

##

struct Node
    tag::Symbol
    val::SubString{String}
    children::NTuple{N, Node} where N
end

##

function _compile_rule(rule::Rule)
    name = QuoteNode(rule.name)
    node = _compile(rule.node)

    quote
        function $(rule.name)(str, idx)
            # get!(cache, idx) do
                res = $(node)(str, idx)

                isnothing(res) && return nothing

                match, offset = res

                return Node($(name), SubString(str, idx, offset), match), offset
            # end
        end
    end |> unblock
end

function _compile(str::STR)
    pat = str.pattern
    len = length(pat)

    :((str, idx, _) -> startswith(@view str[idx:end], $(pat)) ? ((), $(len)) : nothing)
end

function _compile(rgx::RGX)
    pat = rgx.pattern

    quote
        function (str, idx)
            res = Base.match($(pat), str, idx)

            if isnothing(res)
                return nothing
            else
                return (), length(res.match)
            end
        end
    end |> unblock
end

function _compile(sym::SYM)
    return sym.rulename
end

function _compile(seq::SEQ)
    op1, op2 = map(_compile, seq.children)

    quote
        function (str, idx)
            r1 = $(op1)(str, idx)

            isnothing(r1) && return nothing

            m1, o1 = r1

            r2 = $(op2)(str, idx + o1)

            isnothing(r2) && return nothing

            m2, o2 = r2

            return (m1..., m2...), o1 + o2 
        end
    end |> unblock
end

function _compile(alt::ALT)
    op1, op2 = map(_compile, alt.children)

    quote
        function (str, idx)
            res = $(op1)(str, idx)

            isnothing(res) || return res

            return $(op2)(str, idx)
        end
    end |> unblock
end

end