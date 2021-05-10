(TeX-add-style-hook
 "phys4000_workshop_1"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "sec:lagu-radi-basis-code"
    "sec:lagu-radi-basis-figs"
    "fig:laguerre-0-1.0"
    "sec:kinet-energy-matr"
    "sec:extens-overl-matr"
    "sec:atom-hydr-stat"
    "sec:extens-he+-ion"
    "sec:extens-surface-plot-xz"
    "sec:extens-numer-calc-potent")
   (LaTeX-add-listings-lstdefinestyles
    "ff"))
 :latex)

