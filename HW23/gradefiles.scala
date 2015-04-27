/*Created by Tom Biju*/
/*Thanks to G.K. Thiruvathukal for useful code utilized in this homework, thanks to Griffin for being awesome and writing Scala instructions, grading the homework*/
import scala.util._
import scala.io._	//allows for reading files
import scala.math._	
import java.io._	//allows for writing to files

object gradefiles extends App {

  def parseCSVHeader(line : String) : Array[String] = {	//function to parse a line of strings, getting rid of  commas and spaces
    val tokens = line.split(",")
    for (i <- 0 until tokens.length)
       tokens(i) = tokens(i).trim
    tokens
  }

  def parseCSVRowOfDoubles(line : String, failValue : Double) : Array[Double] = {	//function to parse double values in the file information

    val tokens = line.split(",")
    val doubles = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      doubles(i) = Try(tokens(i).trim.toDouble) getOrElse(failValue)
    } 
    doubles
  }

  def parseCSVRowOfInts(line : String, failValue : Int) : Array[Int] = {	//function to parse line of integers

    val tokens = line.split(",")
    val integers = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      integers(i) = Try(tokens(i).trim.toInt) getOrElse(failValue)
    } 
    integers
  }

  def readCategoryFile(courseName : String) : (Int, Array[String], Array[Int], Array[Int]) = {	//function to reaad the categories file
    val courseFileName = s"categories_$courseName.txt"
    val file = Source.fromFile(courseFileName)
    val lines = file.getLines
    val header = lines.next
    val headerNames = parseCSVHeader(header)
    val weights = lines.next
    val weightsArray = parseCSVRowOfInts(weights, -1)
	val quantities = lines.next
    val quantitiesArray = parseCSVRowOfInts(quantities, -1)

    val columns = min(min(headerNames.length, quantitiesArray.length), weightsArray.length)

    (columns, headerNames, weightsArray, quantitiesArray)
  }
	def readCourseStudents(courseName:String):(Array[String],Array[String],Array[String])={	//function to read the information about the students in the course
	 val courseFileName = s"students_$courseName.txt"
     val file = Source.fromFile(courseFileName)
     val lines = file.getLines
	 var IDArray=new Array[String](10)
	 var IDIterator=0
	 var lastNameArray=new Array[String](10)
	 var lastIterator=0
	 var firstNameArray=new Array[String](10)
	 var firstIterator=0
	 var kingballer="TBD"
	 while(kingballer=="TBD"){
		 if(lines.hasNext){
			 var temp=lines.next
			 var tempArray=parseCSVHeader(temp)
			 for(a<-0 until tempArray.length){
				 if(a==0){
					 IDArray(IDIterator)=tempArray(a)
					 IDIterator+=1
				 }
				 else if(a==1){
					 lastNameArray(lastIterator)=tempArray(a)
					 lastIterator+=1
				 }
				 else if(a==2){
					 firstNameArray(firstIterator)=tempArray(a)
					 firstIterator+=1
				 }
			 }
		 }
		 else{
			 kingballer="Tom Biju"
		 }
	 }
		(IDArray,lastNameArray,firstNameArray)
	}
	def readIndividualScores(ID:String,courseName:String):(Array[String],Array[String],Array[String],Array[String],Array[String],Array[String],Array[String],Array[String],Array[String],Array[String])={
	//function to read the coursework information of individual students 
	val courseFileName = s"$ID$courseName.data"
     val file = Source.fromFile(courseFileName)
     val lines = file.getLines
	 var HomeworkScoresArray=new Array[String](20)
	 var ExamsScoresArray=new Array[String](20)
	 var ProjectScoresArray=new Array[String](20)
	 var LabsScoresArray=new Array[String](20)
	 var classParticipationScoresArray=new Array[String](20)
	 var homeworkNumbers=new Array[String](5)
	 var examsNumbers=new Array[String](5)
	 var projectNumbers=new Array[String](5)
	 var labsNumbers=new Array[String](5)
	 var classParticipationNumbers=new Array[String](5)
	 var kingballer="unknown"
	 var CPIterator=0
	 var HIterator=0
	 var EIterator=0
	 var PIterator=0
     var LIterator=0
	 while(kingballer=="unknown"){
		if(lines.hasNext){
			 var temp=lines.next
	 		 var tempArray=temp.split(", ")
			 if(tempArray(0)=="Homework"){
				 HomeworkScoresArray(HIterator)=tempArray(2)
				 homeworkNumbers(HIterator)=tempArray(1)
				 HIterator+=1
			 }
			 else if(tempArray(0)=="Exams"){
				 ExamsScoresArray(EIterator)=tempArray(2)
				 examsNumbers(EIterator)=tempArray(1)
				 EIterator+=1
			 }
			 else if(tempArray(0)=="Project"){
				 ProjectScoresArray(PIterator)=tempArray(2)
				 projectNumbers(PIterator)=tempArray(1)
				 PIterator+=1
			 }
			 else if(tempArray(0)=="Labs"){
				 LabsScoresArray(LIterator)=tempArray(2)
				 labsNumbers(LIterator)=tempArray(1)
				 LIterator+=1
			 }
			 else if(tempArray(0)=="Class Participation"){
				 classParticipationScoresArray(CPIterator)=tempArray(2)
				 classParticipationNumbers(CPIterator)=tempArray(1)
				 CPIterator+=1
			 }
		}
	  else{
		    kingballer="Tom Biju"
	        }
	 }
		 (HomeworkScoresArray,ExamsScoresArray,ProjectScoresArray,LabsScoresArray,classParticipationScoresArray,homeworkNumbers,examsNumbers,projectNumbers,labsNumbers,classParticipationNumbers)
	}
	def gradeCalculator(homeworkArray:Array[String], examArray:Array[String], projectArray:Array[String], labArray:Array[String],participationArray:Array[String], weightsArray:Array[Int],quantitiesArray:Array[Int], headerNames:Array[String]):Double={
		//function to use many returned values as parameters to calculate the weighted grade
		var sum=0.0
		var totalHomeworkScore=0.0
		var totalExamScore=0.0
		var totalProjectScore=0.0
		var totalLabScore=0.0
		var totalParticipationScore=0.0
		var homeworkIndex=0
		var examIndex=0
		var projectIndex=0
		var labIndex=0
		var participationIndex=0
		var grade=0.0
		var totalWeight=0
		for(dovedarkchocolate<-0 to headerNames.length-1){
			if(headerNames(dovedarkchocolate)=="Homework"){
				homeworkIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Exams"){
				examIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Project"){
				projectIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Labs"){
				labIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Class Participation"){
				participationIndex=dovedarkchocolate
			}
		}
		if(homeworkArray(0)!=null){
		for(a<-0 to homeworkArray.length-1){
			if(homeworkArray(a)!=null){
			sum+=homeworkArray(a).toInt
			}
		}
		totalHomeworkScore=((sum/quantitiesArray(homeworkIndex).toDouble)*(weightsArray(homeworkIndex).toDouble/100.0))
		sum=0
		}
		if(examArray(0)!=null){
		for(b<-0 to examArray.length-1){
			if(examArray(b)!=null){
			sum+=examArray(b).toInt
			}
		}
		totalExamScore=((sum/quantitiesArray(examIndex).toDouble)*(weightsArray(examIndex).toDouble/100.0))
		sum=0
		}
		if(projectArray(0)!=null){
		for(c<-0 to projectArray.length-1){
			if(projectArray(c)!=null){
			sum+=projectArray(c).toInt
			}
		}
		totalProjectScore=((sum/quantitiesArray(projectIndex).toDouble)*(weightsArray(projectIndex).toDouble/100.0))
		sum=0
		}
		if(labArray(0)!=null){
			for(d<-0 to labArray.length-1){
				if(labArray(d)!=null){
				sum+=labArray(d).toInt
				}
			}
		    totalLabScore=((sum/quantitiesArray(labIndex).toDouble)*(weightsArray(labIndex).toDouble/100.0))
			sum=0
		}
		if(participationArray(0)!=null){
			for(e<-0 to participationArray.length-1){
				if(participationArray(e)!=null){
				sum+=participationArray(e).toInt
				}
			}
			totalParticipationScore=((sum/(quantitiesArray(participationIndex).toDouble))*((weightsArray(participationIndex).toDouble)/100.0))
			sum=0
		}
		grade=grade+totalHomeworkScore+totalExamScore+totalLabScore+totalProjectScore+totalParticipationScore
		for(x<-0 to weightsArray.length-1){
			if(weightsArray(x).toString!=null){
				totalWeight+=weightsArray(x)
			}
		}
		grade=(grade/totalWeight)*100
		grade=BigDecimal(grade).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
		(grade)
	}
	def letterGradeCalculator(grade:Double):String={	//function to take the weighted grade as a parameter to return the letter grade
		var letterGrade=""
		if(grade>=93){
			letterGrade="A"
		}
		else if(grade>=90){
			letterGrade="A-"
		}
		else if(grade>=86){
			letterGrade="B+"
		}
		else if(grade>=83){
			letterGrade="B"
		}
		else if(grade>=80){
			letterGrade="B-"
		}
		else if(grade>=76){
			letterGrade="C+"
		}
		else if(grade>=73){
			letterGrade="C"
		}
		else if(grade>=70){
			letterGrade="C-"
		}
		else if(grade>=66){
			letterGrade="D+"
		}
		else if(grade>=63){
			letterGrade="D"
		}
		else if(grade>=60){
			letterGrade="D-"
		}
		else{
			letterGrade="F"
		}
		(letterGrade)
	}
	def missingWorkFinder(homeworkNumbers:Array[String],examsNumbers:Array[String], projectNumbers:Array[String],labsNumbers:Array[String],classParticipationNumbers:Array[String],quantitiesArray:Array[Int], headerNames:Array[String]): String={
		//function to check if individual students have missign coursework, and to return the information pertaining to the missing work if there is any missing work
		var missing=" Missing:"
		var homeworkIndex=0
		var examIndex=0
		var projectIndex=0
		var labIndex=0
		var participationIndex=0
		var flag="DNE"
		var flag2="false"
		var numMissing=0
		for(dovedarkchocolate<-0 to headerNames.length-1){
			if(headerNames(dovedarkchocolate)=="Homework"){
				homeworkIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Exams"){
				examIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Project"){
				projectIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Labs"){
				labIndex=dovedarkchocolate
			}
			else if(headerNames(dovedarkchocolate)=="Class Participation"){
				participationIndex=dovedarkchocolate
			}
		}
					if(labsNumbers(0)!=null){
					for(missing<-1 to quantitiesArray(labIndex).toInt){
						for(available<-0 until labsNumbers.length){
							if(labsNumbers(available)!=null){
							if(missing==labsNumbers(available).toInt){
								flag="exists"
							}
							}
						}
						if(flag!="exists"){
							numMissing+=1
							flag2="true"
						}
						flag="DNEL"
					}
					if(flag2=="true"){
						missing+=(" "+numMissing.toString)
						missing+=" L"
						numMissing=0
					}
					}
					flag2="false"
					if(homeworkNumbers(0)!=null){
					for(missing<-1 to quantitiesArray(homeworkIndex).toInt){
						for(available<-0 until homeworkNumbers.length){
							if(homeworkNumbers(available)!=null){
							if(missing==homeworkNumbers(available).toInt){
								flag="exists"
							}
							}
						}
						if(flag!="exists"){
							numMissing+=1
							flag2="true"
						}
						flag="DNEH"
					}
					if(flag2=="true"){
						missing+=(" "+numMissing.toString)
						missing+=" H"
						numMissing=0
					}
					}
					flag2="false"
					if(projectNumbers(0)!=null){
					for(missing<-1 to quantitiesArray(projectIndex).toInt){
						for(available<-0 until projectNumbers.length){
							if(projectNumbers(available)!=null){
							if(missing==projectNumbers(available).toInt){
								flag="exists"
							}
							}
						}
						if(flag!="exists"){
							numMissing+=1
							flag2="true"
						}
						flag="DNEP"
					}
					if(flag2=="true"){
						missing+=(" "+numMissing.toString)
						missing+=" P"
						numMissing=0
					}
					}
					flag2="false"
					if(examsNumbers(0)!=null){
					for(missing<-1 to quantitiesArray(examIndex).toInt){
						for(available<-0 until examsNumbers.length){
							if(examsNumbers(available)!=null){
							if(missing==examsNumbers(available).toInt){
								flag="exists"
							}
							}
						}
						if(flag!="exists"){
							numMissing+=1
							flag2="true"
						}
						flag="DNEE"
					}
					if(flag2=="true"){
						missing+=(" "+numMissing.toString)
						missing+=" H"
						numMissing=0
					}
					}
					flag2="false"
					if(classParticipationNumbers(0)!=null){
					for(missing<-1 to quantitiesArray(participationIndex).toInt){
						for(available<-0 until classParticipationNumbers.length){
							if(classParticipationNumbers(available)!=null){
							if(missing==classParticipationNumbers(available).toInt){
								flag="exists"
							}
							}
						}
						if(flag!="exists"){
							numMissing+=1
							flag2="true"
						}
						flag="DNECP"
					}
					if(flag2=="true"){
						missing+=(" "+numMissing.toString)
						missing+=" CP"
						numMissing=0
					}
					}
		(missing)
	}
	def classGradeReport(courseName:String,lastNameArray:Array[String],firstNameArray:Array[String], gradeArray:Array[String],letterGradeArray:Array[String],missingLabelArray:Array[String]){
		//function to write the students last name, first name, weighted grade, letter grade, and missing work if applicable
		val file1 = new File(courseName+"_summary.txt")
		val bw = new BufferedWriter(new FileWriter(file1))
		for(student<-0 to lastNameArray.length-1){
			if(lastNameArray(student)!=null){
			bw.write(lastNameArray(student)+", "+firstNameArray(student)+" "+gradeArray(student)+" "+letterGradeArray(student))
			if(missingLabelArray(student)!=" Missing:"){
				bw.write(missingLabelArray(student))
			}
			bw.write("\n")
		}
		}
		bw.close()
	}
  //val exampleHeading = "Exams, Homework, Project"
  //val exampleData = "10, a, 25, 35.6"
  //val exampleData2 = "10, a, 25, 35"

 // println(">> Heading")
 // parseCSVHeader(exampleHeading) foreach println
 // println(">> CSV Row of Doubles")
  //parseCSVRowOfDoubles(exampleData, -1.0) foreach println
 // println(">> CSV Row of Ints")
  //parseCSVRowOfInts(exampleData2, -1) foreach println

  //println(">> Reading the Category File")
  val courseName = Try(args(0)) getOrElse("comp150")
  println(s">> Reading $courseName categories file")
  val students=readCourseStudents(courseName)
   students match {
    case (i, l, f) => {
      println("IDs")
      i foreach println
      println("Last Names")
      l foreach println
      println("First Names")
      f foreach println
   }
   }
	
  val results = readCategoryFile(courseName)
  results match {
    case (n, h, w, q) => {
      println(s"There are $n columns of data")
      println("Headings")
      h foreach println
      println("Weights")
      w foreach println
      println("Quantities")
      q foreach println
   }
 }
	var gradeArray=new Array[String](21)
	var letterGradeArray=new Array[String](21)
	var missingLabelArray=new Array[String](21)
	println(s">>Calculating individual grade for students in $courseName")
	println(">>Checking for completion of all work")
	for(estudiantes<-0 to (students._1).length-1){
		if(students._1(estudiantes)!=null){
		var individualInfo=readIndividualScores(students._1(estudiantes),courseName)
		var individualGrade=gradeCalculator(individualInfo._1,individualInfo._2,individualInfo._3,individualInfo._4,individualInfo._5,results._3,results._4,results._2)
		gradeArray(estudiantes)=individualGrade.toString
		var gradeLetter=letterGradeCalculator(individualGrade)
		letterGradeArray(estudiantes)=gradeLetter
		var missingLabel=missingWorkFinder(individualInfo._6,individualInfo._7,individualInfo._8,individualInfo._9,individualInfo._10,results._4,results._2)
		missingLabelArray(estudiantes)=missingLabel
		}
	}
	println(s"<<Writing $courseName class data to file")
	classGradeReport(courseName,students._2,students._3,gradeArray,letterGradeArray,missingLabelArray)
	println(">>The end")//THE END 
}
