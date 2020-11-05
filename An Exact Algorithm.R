library(kinship2)
test1 <- data.frame(id =c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                    mom =c(0, 0, 0, 0, 2, 2, 4, 4, 6, 2, 0, 0, 12, 13),
                    dad =c(0, 0, 0, 0, 1, 1, 3, 3, 3, 7, 0, 0, 11, 10),
                    sex =c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1))
tped <- with(test1, pedigree(id, dad, mom, sex))
kinship(tped)
pedigree(test1$id, test1$dad, test1$mom, test1$sex)


A=matrix(0,nrow=14,ncol=14)
for(i in 1:14){
  if(test1[i,'mom']==0 & test1[i,'dad']==0){
    A[i,i]=1
    a=which(diag(A)!=1)
  }
}
# temp=A
for(i in a){
    for(v in 1:14){
      if(v==i){
        A[i,i]=1
      }
      if(v!=i){
        if(A[test1$mom[i],v]==1 | A[test1$dad[i],v]==1){
          # A[test1$mom[i],v]=A[test1$dad[i],v]
          A[i,v]=1
          # A[test1$mom[i],v]=1
          # A[test1$dad[i],v]=1
        }
        # A[test1$mom[i],v]=1
        # A[test1$dad[i],v]=1
        # A[i,v]=1
      }
    }
}


Phi=matrix(1,nrow=14,ncol=14)
for(i in 1:14){
  if(test1$mom[i]==0 & test1$dad[i]==0){
    Phi[i,i]=(1+0.5)/2
  }
}
b=which(test1$mom==0)
c=which(test1$dad==0)
for(i in b){
  for(j in c){
    if(i !=j){
      Phi[i,j]=0
    }
  }
}
f=which(test1$mom==0 & test1$dad==0)
fm=f[which(test1[f,'sex']==1)]
ff=f[which(test1[f,'sex']==0)]
test11<-as.matrix(test1)
cc=test1[-f,'id']
for(j in cc){
  if(test1[j,'mom']%in%f == FALSE & test1[j,'dad']%in%f == FALSE){
    for(fi in f){
      Phi[j,fi]=Phi[fi,j]=0
    }
  }
}
 for(i in 1:14){
   for(j in 1:14){
     if(i==j){
       if(test1$mom[i]==0){
         Phi[i,i]=(1+Phi[i,i])/2
       }
       if(test1$mom[i]!=0){
         Phi[i,i]=(1+Phi[test1[i,'mom'],test1[i,'dad']])/2
       }
     }
     if(i!=j){
       if(test1$mom[i]==0){
         Phi[i,j]=Phi[j,i]=(Phi[i,j]+Phi[i,j])/2
       }
       if(test1$mom[i]!=0){
         Phi[i,j]=Phi[j,i]=(Phi[test1[i,'mom'],j]+Phi[test1[i,'dad'],j])/2
       }
     }
   }
}
 # for(i in 1:14){
 #   Phi[i,i]=(2*Phi[i,i])-1
 # }

 # for(i in cc){
 #   for(j in cc){
 #     if(i==j){
 #       Phi[i,i]=(1+Phi[test1[i,'mom'],test1[i,'dad']])/2
 #     }
 #     if(i!=j){
 #       if(A[i,j]==0){
 #         Phi[i,j]=(Phi[test1[i,'mom'],j]+Phi[test1[i,'dad'],j])/2
 #       }
 #     }
 #   }
 # }
 # for(i in 1:14){
 #   Phi[i,i]=(2*Phi[i,i])-1
 # }

kinshiptest<-function(test){
  n<-nrow(test)
  A=matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    if(test[i,'mom']==0 & test[i,'dad']==0){
      A[i,i]=1
      a=which(diag(A)!=1)
    }
  }
  for(i in a){
    for(v in 1:n){
      if(v==i){
        A[i,i]=1
      }
      if(v!=i){
        if(A[test$mom[i],v]==1 | A[test$dad[i],v]==1){
          # A[test1$mom[i],v]=A[test1$dad[i],v]
          A[i,v]=1
          # A[test1$mom[i],v]=1
          # A[test1$dad[i],v]=1
        }
        # A[test1$mom[i],v]=1
        # A[test1$dad[i],v]=1
        # A[i,v]=1
      }
    }
  }
  Phi=matrix(1,nrow=n,ncol=n)
  for(i in 1:n){
    if(test$mom[i]==0 & test$dad[i]==0){
      Phi[i,i]=(1+0.5)/2
    }
  }
  b=which(test$mom==0)
  c=which(test$dad==0)
  for(i in b){
    for(j in c){
      if(i !=j){
        Phi[i,j]=0
      }
    }
  }
  f=which(test$mom==0 & test$dad==0)
  fm=f[which(test[f,'sex']==1)]
  ff=f[which(test[f,'sex']==0)]
  test111<-as.matrix(test)
  cc=test[-f,'id']
  for(j in cc){
    if(test[j,'mom']%in%f == FALSE & test[j,'dad']%in%f == FALSE){
      for(fi in f){
        Phi[j,fi]=Phi[fi,j]=0
      }
    }
  }
  for(i in 1:14){
    for(j in 1:14){
      if(i==j){
        if(test$mom[i]==0){
          Phi[i,i]=(1+Phi[i,i])/2
        }
        if(test$mom[i]!=0){
          Phi[i,i]=(1+Phi[test[i,'mom'],test[i,'dad']])/2
        }
      }
      if(i!=j){
        if(test$mom[i]==0){
          Phi[i,j]=Phi[j,i]=(Phi[i,j]+Phi[i,j])/2
        }
        if(test$mom[i]!=0){
          Phi[i,j]=Phi[j,i]=(Phi[test[i,'mom'],j]+Phi[test[i,'dad'],j])/2
        }
      }
    }
  }
  return(Phi)
}

Phi1<-kinshiptest(test1)


colnames(testped1)<-c("id","dad","mom","sex")
Phi_ped1<-kinshiptest(testped1)


tped11 <- with(testped1, pedigree(id, dad, mom, sex))
kinship(tped11)

install.packages("synbreed")
Phi_AILped<-kinshiptest(AILped)
AILped1 <- with(AILped, pedigree(id, dad, mom, sex))
Phi_AILped1<-kinship(AILped1)

AILped0 <- AILped[ , c("id", "dad", "mom", "sex")]
# AILped1<-AILped0[order(AILped0[,1],decreasing=F),]
# nrows(AILPEd1)
library(dplyr)
AILped1<-arrange(AILped0, id)



AILped0 <- with(AILped0, pedigree(id, dad, mom, sex))
l1<-which(AILped$sex==0)
l2<-which(AILped$sex==1)
AILped$sex[l1]=1
AILped$sex[l2]=0
AILped12<-arrange(AILped, id)
AILped12 <- AILped12[ , c("id", "dad", "mom", "sex")]
AILped12 <- with(AILped12, pedigree(id, dad, mom, sex))
Phi_AILped12<-kinship(AILped12)

ailped <- sim_ail_pedigree(ngen=12, npairs=100, nkids_per=5)
Phi_ailped<-kinshiptest(ailped)
ailped1<-with(ailped,pedigree(id,dad,mom,sex))

data(geneaJi)
genJi<-gen.genealogy(geneaJi)
kinship<-gen.phi(genJi)
kinship

ailped1<-ailped[,c("id","mom","dad","sex")]
colnames(ailped1)<-c("ind","mother","father","sex")
l11<-which(ailped1$sex==0)
l22<-which(ailped1$sex==1)
ailped1$sex[l11]=2
ailped1$sex[l22]=1
gen_ailped<-gen.genealogy(ailped1)
Phi_ailped<-gen.phi(gen_ailped)
