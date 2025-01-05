about_panel <- function() {
  fluidPage(
    withMathJax(),
    div(style = 'margin: 25px 300px 200px 300px',
      div(style = 'margin-top:20px;',
          h1("Verapamil in children's presymptomatic type 1 diabetes: a hybrid Markov model")
      ),
      div(style = 'margin-top:10px; margin-bottom:20px; text-align: justify',
        p(style = 'font-style: italic;',
          "by Marcos Bolaños"
        ),
        p(
          "This health economics model investigates the cost-effectiveness of Verapamil in children's presymptomatic type 1 diabetes. Based on an approach by ",
          a(href = "https://pubmed.ncbi.nlm.nih.gov/32960433/", "Mital S. et al. (2020)"),
          ", we combine a simple decision tree with two variants of a Markov model in order to estimate outcomes and costs of Verapamil treatment for slowing the progression of type 1 diabetes in children.",
          p(
          "The app is implemented in R and Shiny. Its user interface allows to customize a wide range of parameters, both for transparency and to allow exploration of different scenarios. The model estimates costs and quality-adjusted life years (QALYs) for Verapamil treatment compared to no treatment. The results are presented in a table and a cost-effectiveness plane. This model may be used to inform decision-making in healthcare policy."
          ),
          p(
          "The source code for this project is available on",
          a(href = "https://github.com/MarcosQBV", "GitHub"),
          ", where you can also find a",
          a(href = "https://github.com/MarcosQBV/ped-t1d-model/blob/main/step-by-step.ipynb", "notebook"),
           "that helps to understand the model's structure and the calculations behind it, step by step. Part of the code is based on an ",
          a(href = "https://wellcomeopenresearch.org/articles/5-69/v1", "article"),
          "as well as an ",
          a(href = "https://github.com/dark-peak-analytics/sadm-mk2-demo", "open source repository"),
          "by Robert A. Smith and Paul P. Schneider of Dark Peak Analytics.",
          )
          ),
          h1(style="margin-top: 40px; margin-bottom: 20px;",
          "Model structure"),
          p( 
            "The model consists of a decision tree (fig. 1) followed by two similar but distinct Markov models (fig. 2). For each given patient, two scenarios are simulated : one where the patient receives Verapamil treatment and one where the patient receives no treatment.", 
          ),
          div(style = 'margin-top:20px; margin-bottom:20px; text-align: center;',
            img(src = "decision tree.png", alt = "Figure 1", style = 'max-width: 75%; height: auto;  font-size: 1em;'),
            p(style = 'font-style: italic; margin-top: 10px;', "Figure 1: A diagram of the decision tree.")
          ),
          p(style="margin-top: 40px;",
            " A Markov model is run for each scenario, where the patient can transition between three health states: presymptomatic diabetes (Stages 1 or 2), symptomatic diabetes (Stage 3), or death. In the scenario where verapamil is administered, it is considered that the probability of transitioning from stage to to stage 3 is diminished by a given ratio : this is the difference between the two models. "
          ),
          div(style = 'margin-top:80px; margin-bottom:20px; margin-left: 20px; text-align: center;',
            img(src = "markov model.png", alt = "Figure 2", style = 'max-width: 75%; height: auto; font-size: 1em;'),
            p(style = 'font-style: italic; margin-top: 10px;', "Figure 2: The structure of the model at each end of the tree.")
          ),
          p(style="margin-top: 40px;",
            "Since we are utilizing Probabilistic Sensitivity Analysis (PSA), each parameter is assigned a probability distribution. The model is run multiple times, each time with a different set of parameters sampled from the assigned distributions. The results are then aggregated to provide a distribution of costs and QALYs for each scenario. The cost-effectiveness of Verapamil treatment is then assessed by calculating the Incremental Cost-Effectiveness Ratio (ICER) :",
          ),
          p(style="font-size: 1.5em;",
            "$$\\text{ICER} = \\frac{\\Delta C}{\\Delta E}$$"
          ),
          p("Where:"),
          p("\\(\\Delta C\\) is the difference in costs between the intervention and the comparator."),
          p("\\(\\Delta E\\) is the difference in effectiveness (e.g., QALYs) between the intervention and the comparator.")
        )
      )
    )
}