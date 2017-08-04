## Function to input a protein fasta file and output the scaffold IDs

protein2scaff <- function(filename){
  library(seqinr) # for reading in fasta files

# Read in protein fasta file
  prot <- read.fasta(filename)

# Extract the header names and make into a data frame
  prot.df <- data.frame(header= unlist(getAnnot(prot)))

# Remove all characters after the protein ID
  prot.df$protein_id <- gsub("(>)|( .*$)", "", prot.df$header)

# Remove the protein ID to get the scaffold ID
  prot.df$scaffold_name <- unlist(lapply(prot.df$protein_id, function(x) paste(strsplit(x, "_")[[1]][1:4], collapse="_")))
  # See this website for strsplit command: https://stackoverflow.com/questions/7449564/regex-return-all-before-the-second-occurrence

  return(prot.df)
}

