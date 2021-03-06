% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_imp.R
\name{add_imp_plot}
\alias{add_imp_plot}
\title{add bar plot of important features to model response alluvial plot}
\usage{
add_imp_plot(grid, p = NULL, data_input, plot = T, ...)
}
\arguments{
\item{grid}{gtable or ggplot}

\item{p}{alluvial plot, optional if alluvial plot has already been passed as grid.  Default: NULL}

\item{data_input}{dataframe used to generate alluvial plot}

\item{plot}{logical if plot should be drawn or not}

\item{...}{additional parameters passed to \code{\link[easyalluvial]{plot_imp}}}
}
\value{
gtable
}
\description{
adds bar plot of important features to model response alluvial plot
}
\examples{
\dontrun{
df = mtcars2[, ! names(mtcars2) \%in\% 'ids' ]

train = caret::train( disp ~ .
                     , df
                     , method = 'rf'
                     , trControl = caret::trainControl( method = 'none' )
                     , importance = TRUE )

pred_train = caret::predict.train(train, df)

p = alluvial_model_response_caret(train, degree = 4, pred_train = pred_train)

p_grid = add_marginal_histograms(p, data_input = df)

p_grid = add_imp_plot(p_grid, p, data_input = df)
}
}
\seealso{
\code{\link[gridExtra]{arrangeGrob}}
 \code{\link[easyalluvial]{plot_imp}}
}
