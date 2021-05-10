(TeX-add-style-hook
 "phys4000_workshop_1"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "base"
    "color"
    "listings")
   (LaTeX-add-labels
    "sec:introduction"
    "sec:laguerre-basis"
    "sec:recurrence-relation"
    "sec:norm-const"
    "sec:code"
    "sec:kinet-energy-matr"
    "sec:extens-overl-matr"
    "sec:atom-hydr-stat"
    "sec:extens-he+-ion"
    "sec:extens-surface-plot-xz"
    "sec:extens-numer-calc-potent"))
 :latex)

