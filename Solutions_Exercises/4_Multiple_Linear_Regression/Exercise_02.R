library(DiagrammeR)

grViz("
digraph model {
  
  # Graph style
  graph [layout = dot, rankdir = TB]
  
  # Node styles
  node [shape = ellipse, fontname = Helvetica, fontsize = 12]
  
  alpha [label = 'α ~ Normal(178, 20)']
  beta1 [label = 'β₁ ~ Log-Normal(0, 1)']
  beta2 [label = 'β₂ ~ Normal(0, 1)']
  sigma [label = 'σ ~ Uniform(0, 50)']
  
  mu_i [label = 'μᵢ = α + β₁xᵢ + β₂xᵢ²']
  h_i [label = 'hᵢ ~ Normal(μᵢ, σ)']

  # Edges
  alpha -> mu_i
  beta1 -> mu_i
  beta2 -> mu_i
  mu_i -> h_i
  sigma -> h_i
}
")