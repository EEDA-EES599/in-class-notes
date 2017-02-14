H_from_points <- function(fp,tp) {
# Find homography H, such that fp is mapped to tp using the linear DLT method.

#  The points are conditioned by normalizing so that they have zero mean and unit
# standard deviation. This is very important for numerical reasons since the stability
# of the algorithm is dependent of the coordinate representation. 
    
# 'from' points
    
m = apply(fp,1,mean)
m = m[c(1,2)]

maxstd = max(apply(fp, 1, function(fp) sd(fp) * sqrt((length(fp) - 1) / length(fp)) )) + 1e-9
    
C1 = diag(c(1/maxstd, 1/maxstd, 1) )
C1[1, 3] = -m[1]/maxstd
C1[2, 3] = -m[2]/maxstd   

fp = C1 %*% fp 
 
# 'to' points    
m = apply(tp,1,mean)
m = m[c(1,2)]

maxstd = max(apply(tp, 1, function(tp) sd(tp) * sqrt((length(tp) - 1) / length(tp)) )) + 1e-9
    
C2 = diag(c(1/maxstd, 1/maxstd, 1) )
C2[1, 3] = -m[1]/maxstd
C2[2, 3] = -m[2]/maxstd       
tp = C2 %*% tp    
    
# Then the matrix A is created using the point correspondences. 
    
nbr_correspondences = dim(fp)[2]
    
A = matrix(0,2*nbr_correspondences,9)    
    
for (i in seq(nbr_correspondences)){    
        A[2*i - 1,] = c(-fp[1,i],-fp[2,i],-1,0,0,0, tp[1,i]*fp[1,i],tp[1,i]*fp[2,i],tp[1,i])
        A[2*i,] = c(0,0,0,-fp[1,i],-fp[2,i],-1,tp[2,i]*fp[1,i],tp[2,i]*fp[2,i],tp[2,i])
    }
   
# The least squares solution is found as the
# last row of the matrix V of the SVD.     
# carry out the singular value decomposition on matrix A    
s = svd(A, nu = 9, nv = 9)  
    
S = s$d
U = s$u
V = s$v
    
#The row is reshaped to create H .        
# the homography    
H = matrix(V[,9],nrow=3,byrow=T)    

# This matrix is then de-conditioned and normalized before returned. 
#decondition and normalize   
H = solve(C2) %*% (H %*% C1) 
#H = H/H[3,3]
  
    return(H / H[3,3])
    }            

