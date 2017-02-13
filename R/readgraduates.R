#' @title Read Graduates
#' @description  Scan for the names of graduates and create a list of names
#' @param year Sets the year of graduates to be scanned
#' @return A list of names of graduates from \code{year}
#' @usage readgraduates( year )
#' @import stringr
#' @import gender
#' @export
readgraduates <- function( year ){

  # Check parameter
  if ( year < 2000 || year > 2015 ){
    warning("Invalid parameter \"year\", \"year\" must be between 2000-2015." )
    return()
  }

  # Read the data of the year
  input <- readLines(system.file("extdata", str_c(year,".txt"), package = "williamsgraduates"), warn = FALSE)

  # Combine lines that were not supposed to be separated
  for(i in 1:length(input)){
    if ( grepl( ", with", input[i] ) ){
      if ( ! grepl( "Statistics", input[i] ) && ! grepl( "Spanish", input[i] ) &&
           ! grepl( "Sociology", input[i] ) && ! grepl( "Russian", input[i] ) &&
           ! grepl( "Religion", input[i] ) && ! grepl( "Psychology", input[i] ) &&
           ! grepl( "Economy", input[i] ) && ! grepl( "Physics", input[i] ) &&
           ! grepl( "Philosophy", input[i] ) && ! grepl( "Music", input[i] ) &&
           ! grepl( "Mathematics", input[i] ) && ! grepl( "Japanese", input[i] ) &&
           ! grepl( "History", input[i] ) && ! grepl( "Geosciences", input[i] ) &&
           ! grepl( "German", input[i] ) && ! grepl( "French", input[i] ) &&
           ! grepl( "Policy", input[i] ) && ! grepl( "English", input[i] ) &&
           ! grepl( "Economics", input[i] ) && ! grepl( "Science", input[i] ) &&
           ! grepl( "Literature", input[i] ) && ! grepl( "Classics", input[i] ) &&
           ! grepl( "Chinese", input[i] ) && ! grepl( "Chemistry", input[i] ) &&
           ! grepl( "Biology", input[i] ) && ! grepl( "Astrophysics", input[i] ) &&
           ! grepl( "Astronomy", input[i] ) && ! grepl( "Art", input[i] ) &&
           ! grepl( "Theatre", input[i] ) && ! grepl( "Studies", input[i] ) &&
           ! grepl( "Neuroscience", input[i] ) && ! grepl( "Anthropology", input[i] ) ){

        input[i] <- str_c(input[i]," ",input[i+1])
        input <- input[-(i+1)]

      }
    }
  }

  # Separate first names, middle names and last names
  # df: Last Name, First Name, major, Department Honors, Latin Honors
  l <- length(input) - 4
  students = data.frame( "lastName" = vector("character", length= l ),
                         "firstName" = vector("character", length= l ),
                         "major" = vector("character", length= l ),
                         "departmentHonors" = vector("character", l ),
                         "latinHonors" = vector("character", l ),
                         "gender" = vector("character", l ),
                         stringsAsFactors=FALSE
                         )
  latinHonor <- "Summa Cum Laude"
  count <- 1
  for ( i in 2:length(input) ){

    # Latin Honors
    if ( grepl( "Magna Cum Laude", input[i] ) ){
      latinHonor <- "Magna Cum Laude"
      next
    } else if ( grepl( "Cum Laude", input[i] ) ){
      latinHonor <- "Cum Laude"
      next
    } else if ( grepl( "Bachelor of Arts", input[i] ) ){
      latinHonor <- NA
      next
    }

    # Department Honors, major and full name
    if ( grepl( ", with highest honors", input[i] ) ){
      departmentHonor <- "Highest Honors"
      major <- str_sub(input[i], start = str_locate(input[i]," honors in ")[2]+1)
      name <- str_sub(input[i], end = str_locate(input[i],", with")[1]-1)
    } else if ( grepl( ", with honors", input[i] ) ){
      departmentHonor <- "Honors"
      major <- str_sub(input[i], start = str_locate(input[i]," honors in ")[2]+1)
      name <- str_sub(input[i], end = str_locate(input[i],", with")[1]-1)
    } else {
      departmentHonor <- NA
      major <- NA
      name <- input[i]
    }

    # Splits full name to first and last name
    firstName <- word( name )
    lastName <- word( name, -1 )

    # Do not include suffixes in lastName
    if ( lastName == "II" || lastName == "III" || lastName == "IV" ||
         lastName == "V" || lastName == "VI" || lastName == "Jr." ){
      lastName <- word( name, -2 )
    }

    # Deletes * and + from first name
    if ( ! grepl('^[A-Za-z]+$', substring(firstName,1,1)) ){
      firstName <- str_sub(firstName,2)
    }
    if ( ! grepl('^[A-Za-z]+$', substring(firstName,1,1)) ){
      firstName <- str_sub(firstName,2)
    }

    # Estimate gender of student
    gender <- gender(firstName, method="ssa", countries="United States")
    if (is.na(is.na(gender)[1]))
      gender <- NA
    else
      gender <- gender[[4]]
    # For faster estimation, use code below instead
    #gender <- gender( firstName, method = "kantrowitz")[[2]]

    # Record in Data Frame
    students[[1]][[count]] <- lastName
    students[[2]][[count]] <- firstName
    students[[3]][[count]] <- major
    students[[4]][[count]] <- departmentHonor
    students[[5]][[count]] <- latinHonor
    students[[6]][[count]] <- gender
    count <- count + 1

  }

  students
}
