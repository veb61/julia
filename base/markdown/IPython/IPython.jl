type LaTeX
  formula::UTF8String
end

@trigger '$' ->
function latex(stream::IO)
  println("latexing")
  result = parse_inline_wrapper(stream, "\$", repeat = false)
  return result == nothing ? nothing : LaTex(result)
end
