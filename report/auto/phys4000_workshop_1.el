(TeX-add-style-hook
 "phys4000_workshop_1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "draft")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "base")
   (LaTeX-add-labels
    "sec:introduction"
    "sec:laguerre-basis"
    "sec:kinet-energy-matr"
    "sec:extens-overl-matr"
    "sec:atom-hydr-stat"
    "sec:he+-ion"
    "sec:surface-plot-xz"
    "sec:numer-calc-potent"))
 :latex)

