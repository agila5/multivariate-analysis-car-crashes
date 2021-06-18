# Info:
cat("Running utils\n")

# Manually define the spatial random effect in model (G)
my.inla.rgeneric.MCAR.model_a1_ak_Lambda <- function(
  cmd = c(
    "graph", "Q", "mu", "initial", "log.norm.const", "log.prior", "quit"
  ),
  theta = NULL
) {
  ## MCAR implementation MCAR(alpha1, ..., alphak, Lambda) ->
  ## ->  PROPER CAR, alpha1, ..., alphak, dense Lambda
  ## k: number of diseases/blocks
  ## W: adjacency matrix
  ## alpha.min: minimum value for alpha1, ..., alphak
  ## alpha.max: maximum value for alpha, ...., alphak

  # theta: k correlation parameters alpha1, ..., alphak,
  # (k - 1) * k / 2  parameters for lower-tri matrix by col.

  interpret.theta <- function() {
    # Function for changing from internal scale to external scale

    # First k parameters are the autocorrelation parameters
    alpha <- vapply(
      theta[1:k],
      function(x) alpha.min + (alpha.max - alpha.min) * stats::plogis(x),
      numeric(1)
    )
    # The next k parameters are the marginal precisions
    mprec <- vapply(
      theta[(k + 1):(2 * k)],
      exp,
      numeric(1)
    )
    # the last (k * (k - 1)) / 2 are the correlation parameters ordered by columns.
    corre <- vapply(
      theta[(2 * k + 1):(k * (k + 3) / 2)],
      function(x) 2 * stats::plogis(x) - 1,
      numeric(1)
    )

    param <- c(alpha, mprec, corre)

    # length non-diagonal elements
    n <- (k - 1) * k / 2

    # intial matrix with 1s at the diagonal
    M <- diag(1, k)

    # Adding correlation parameters (lower.tri) and (upper.tri)
    M[lower.tri(M)] <- param[(2 * k + 1):(k * (k + 3) / 2)]
    M[upper.tri(M)] <- t(M)[upper.tri(M)]

    # Preparing the st. dev matrix
    st.dev <- sqrt(1 / param[(k + 1):(2 * k)])

    # Matrix of st. dev.
    st.dev.mat <- matrix(st.dev, ncol = 1) %*% matrix(st.dev, nrow = 1)

    # Final inversed matrix
    M <- M * st.dev.mat

    # Inverting matrix
    PREC <- solve(M)

    return(list(alpha = alpha, param = param, VACOV = M, PREC = PREC))
  }

  # Graph of precision function; i.e., a 0/1 representation of precision matrix
  graph <- function() {
    # Build the blockdiagonal matrix
    Rs <- vector("list", length = k)
    D_W <- Matrix::Diagonal(n = nrow(W), x = Matrix::rowSums(W))
    for (j in seq_len(k)) {
      # I choose alpha = 0.5 for no particular reason
      R <- t(chol(D_W - 0.5 * W))
      Rs[[j]] <- R
    }
    Bdiag_R <- Matrix::bdiag(Rs)

    # Build the central part of the matrix product
    central_block <- Matrix::kronecker(
      matrix(1, nrow = k, ncol = k),
      Matrix::Diagonal(nrow(W), 1)
    )

    G <- Bdiag_R %*% central_block %*% t(Bdiag_R)
    G
  }

  Q <- function() {
    # Parameters in model scale
    param <- interpret.theta()

    # Build the blockdiagonal matrix
    Rs <- vector("list", length = k)
    D_W <- Matrix::Diagonal(n = nrow(W), x = Matrix::rowSums(W))
    for (j in seq_len(k)) {
      R <- t(chol(D_W - param$alpha[j] * W))
      Rs[[j]] <- R
    }
    Bdiag_R <- Matrix::bdiag(Rs)

    # Build the central part of the matrix product
    central_block <- Matrix::kronecker(param$PREC, Matrix::Diagonal(nrow(W), 1))

    Q <- Bdiag_R %*% central_block %*% t(Bdiag_R)
    Q
  }

  # Mean of model
  mu <- function() {
    return(numeric(0))
  }

  log.norm.const <- function() {
    ## return the log(normalising constant) for the model
    val <- numeric(0)
    return(val)
  }

  log.prior <- function() {
    ## return the log-prior for the hyperparameters.
    ## Uniform prior in (alpha.min, alpha.max) on model scale
    param <- interpret.theta()

    # log-Prior for the autocorrelation parameter
    val <- 0
    for (j in 1:k) {
      val <- val - theta[j] - 2 * log(1 + exp(-theta[j]))
    }

    # Whishart prior for joint matrix of hyperparameters
    val <- val + log(MCMCpack::dwish(W = param$PREC, v = k, S = diag(rep(lambda_wish, k))))
    # This is for precisions
    val <- val + sum(theta[(k + 1):(2 * k)])
    # This is for correlation terms
    val <- val + sum(
      log(2) + theta[(2 * k + 1):(k * (k + 3) / 2)] -
        2 * log(1 + exp(theta[(2 * k + 1):(k * (k + 3) / 2)]))
    )

    return(val)
  }

  initial <- function() {
    ## return initial values
    # The Initial values form a diagonal matrix
    return(c(rep(0, k), rep(log(1), k), rep(0, (k * (k - 1) / 2))))
  }

  quit <- function() {
    return(invisible())
  }

  val <- do.call(match.arg(cmd), args = list())
  return(val)
}
