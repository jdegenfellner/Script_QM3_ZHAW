\documentclass{standalone}
\usepackage{tikz}
\usetikzlibrary{positioning, shapes.geometric, calc}

\begin{document}

\begin{tikzpicture}[
    node distance=1.8cm and 3.5cm,
    latent/.style={draw, ellipse, align=center, minimum width=4.5cm, font=\normalsize},
    observed/.style={draw, rectangle, align=center, minimum width=4.5cm, font=\normalsize}
]

% Hyperparameter nodes
\node[latent] (mualpha) {$\mu_\alpha \sim \mathrm{Normal}(66, 20)$};
\node[latent, right=of mualpha] (sigmaalpha) {$\sigma_\alpha \sim \mathrm{Exp}(0.5)$};
\node[latent, right=of sigmaalpha] (sigmabeta) {$\sigma_\beta \sim \mathrm{Exp}(1)$};
\node[latent, right=of sigmabeta, yshift=-7cm] (sigmaeps) {$\sigma_\varepsilon \sim \mathrm{Exp}(1)$};

% Latent variables
\node[latent, below=of $(mualpha)!0.5!(sigmaalpha)$] (alphaID) {$\alpha[\mathrm{ID}] \sim \mathrm{Normal}(\mu_\alpha, \sigma_\alpha)$};
\node[latent, below=of $(sigmabeta)!0.5!(sigmaeps)$] (betaRater) {$\beta[\mathrm{Rater}] \sim \mathrm{Normal}(0, \sigma_\beta)$};

% Linear predictor
\node[latent, below=of $(alphaID)!0.5!(betaRater)$] (mu) {$\mu_i = \alpha[\mathrm{ID}] + \beta[\mathrm{Rater}]$};

% Observed outcome
\node[observed, below=of mu] (ROMi) {$ROM_i \sim \mathrm{Normal}(\mu_i, \sigma_\varepsilon)$};

% Arrows
\draw[->, thick] (mualpha) -- (alphaID);
\draw[->, thick] (sigmaalpha) -- (alphaID);
\draw[->, thick] (sigmabeta) -- (betaRater);
\draw[->, thick] (sigmaeps) -- (ROMi);

\draw[->, thick] (alphaID) -- (mu);
\draw[->, thick] (betaRater) -- (mu);
\draw[->, thick] (mu) -- (ROMi);

\end{tikzpicture}

\end{document}