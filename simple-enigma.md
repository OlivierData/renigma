---
title: "R Notebook"
output: html_notebook
---

This notebook is a try to solve a simple enigma.

## Enigma
<img src="percentage.png" alt="enigma"/>
<br /><br />
Find solutions of this equation $A\% . BCD = EF$ for $\{A, B, C, D, E, F\} \in \{0, 1, 2, 3, 4, 5, 6, 7, 8, 9\}$ and $A \not= B \not= C \not= D \not= E \not= F$ 

## Solution verification
```{r}
verif_solution <- function(solution, verbose=FALSE){
  
  xA <- solution[1]/100
  xB <- solution[2]*100 + solution[3]*10 + solution[4]
  xC <- solution[5]*10 + solution[6]
  
  verif <- xA * xB == xC
  
  if(verbose){
    message(paste0(solution[1], "% x ",  xB," = ", xC))
  }
  
  return(verif)
}
```

## Brute force search algorithm
```{r}
brute_force_search <- function(maxtry=10^6, tryall=FALSE, verbose=FALSE){
  
  digits <- 0:9
  npick <- 6
  verif <- FALSE
  ntry <- 0
  nsol <- 0
  solutions <- c()
  
  while(!verif){
    ntry <- ntry + 1
    solution <- sample(digits, npick)
    verif <- verif_solution(solution)
    
    if(tryall){
      if(verif){
        solutions <- rbind(solutions, solution)
        dimnames(solutions)[[1]][dim(solutions)[1]] <- ntry
        solutions <- unique.array(solutions)
        if(verbose){
          if(nsol < dim(solutions)[1]){
            message(paste0("Found ", dim(solutions)[1]), 
                    " solutions at iteration ", ntry)
            nsol <- dim(solutions)[1]
          }
        }
      }
      verif <- FALSE
    }else{
      solutions <- solution
    }
    
    if(ntry>=maxtry){
      verif <- TRUE
    }
    
  }

  return(solutions)
}
```


## Find one solution
```{r}
solution <- brute_force_search()
verif_solution(solution, verbose = TRUE)
```

## Try to find all solutions
```{r}
max_attempts <- 10^6
solution <- brute_force_search(maxtry=max_attempts, tryall = TRUE, verbose = TRUE)
n_solutions_found <- dim(solution)[1]
solution
```


## Theoretical solutions
The number of solutions is limited. We know we can only choose 6 unique digits from a set of 10 elements. Indeed there is n(n-1)…(n-k+1) / k! combinaisons to multiply with !k to get the number of permutations. Among theses permutations, there are some true answers.
```{r}
perm <- function(n,k){choose(n,k) * factorial(k)}
n_permutations <- perm(10, 6)
message(paste("We have", n_permutations, "possibilities to check"))

```

## Expectation to find all solutions
```{r}
iterations_expectation <- c()
for(i in n_solutions_found:1){
  solution_i_iteration_expectation <- n_permutations/i
  iterations_expectation <- c(iterations_expectation, solution_i_iteration_expectation)
}
message(paste("We can find all solutions with around", 
              round(sum(iterations_expectation)), "iterations"))
```

## Statistical result plot function
```{r}
plot_solution <- function(xsol, nmax, theoretical_sol){
  
  x <- c(as.integer(dimnames(xsol)[[1]]), nmax)
  y <- c(1:dim(xsol)[1], dim(xsol)[1])
  z <- c(cumsum(theoretical_sol), nmax)
  hardness <- data.frame(attempts=x, solutions=y, theoretical_iterations=z)
  nsol <- dim(xsol)[1]
  
  title <- paste0("Equation [A]% x [BCD] = [EF] for ![ABCDEF] in [0-9] converges to ", 
                  nsol, " solutions")
  graph <- ggplot2::ggplot(hardness, aes(attempts, solutions))+
    ggplot2::geom_step()+
    ggplot2::geom_line(aes(x=theoretical_iterations), col="steelblue")+
    ggplot2::ggtitle(title)
  
  return(graph)
}
```

## Statistical convergence
```{r fig.width=10}
plot_solution(solution, max_attempts, iterations_expectation)
```

## Generate all permutations
```{r}
#possibilities <- t(utils::combn(0:9, 6)) # combinaisons
possibilities <- gtools::permutations(n=10, r=6, v=0:9) # permutations
# Dimensions of possibilities
print(dim(possibilities))
# 10 first possibilities
head(possibilities, n=10)
```

## Among possibilities search algorithm
```{r}
among_possibilities_search <- function(possibilities){
  solutions <- c()
  for(i in 1:nrow(possibilities)){
    solution <- possibilities[i,]
    verif <- verif_solution(solution)
    if(verif){
      solutions <- rbind(solutions, solution)
    }
  }
  dimnames(solutions)[[1]] <- 1:dim(solutions)[1]
  dimnames(solutions)[[2]] <- LETTERS[1:6]
  return(solutions)
}
```

## Find true solutions among all permutations
```{r}
true_solution <- among_possibilities_search(possibilities)
message(paste("Found", dim(true_solution)[1], 
              "different solutions among theoretical possibilities"))
true_solution
```

