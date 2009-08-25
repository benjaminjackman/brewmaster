package biz.jackman.brewmaster.sample.food



import groovy.lang.GroovyShell
import com.google.inject.Guice
import biz.jackman.brewmaster._
import biz.jackman.brewmaster.classtree.ClassTreeGenerator
import biz.jackman.brewmaster.inject.{GuiceBrewjector, StandardBrewverter}
import biz.jackman.brewmaster.phases.Brewsetter
import org.scalatest.Suite

class TestFood extends Suite {
  val script1 = 
    """def builder = new NodeBuilder()
  	  |root = builder.Employees {
  	  |	 employees {
  	  |    Cook($key : "EMERIL", name : "Emeril", catchphrase : "Bam!")
  	  |	   Waiter($key : "SIMONE", name: "Simone", tips : 5.01d)
  	  |	   Waiter($key : "GOMBERG", name: "Gomberg", tips : strDbl("5.01"))
      |    FunctionalProgrammer($key : "JACKMAN", name : "Ben")
      |    for (i in 1..5) {
      |      Waiter($key : "DENNYS_CLONE_$i".toString(), name : "Denny's Clone $i".toString())
      |    }
  	  |    FunctionalProgrammer($key : "FoobariusMaximus", name : "FooBarius Maximus", favoriteLanguage : "lolcode")
      |  }
  	  |  programmer {
  	  |    FunctionalProgrammer(name : "Odersky", favoriteLanguage : "Pizza", strength : 5)
  	  |  }
  	  |}
  	  |root
      |""".stripMargin
  
  def testEmployeesInitOk {
    //Shows how to make a shell with a method defined
	  implicit val shell = {
	    val shell = new GroovyShell()
	    object Fns {
	      def strDbl(str : String) = str.toDouble
	    }
      import org.codehaus.groovy.runtime.MethodClosure
      import groovy.lang.Binding
	    val strDbl = new MethodClosure(Fns,"strDbl")
      val binding = new Binding()
      shell.setVariable("strDbl", strDbl)
	    shell
	  }
    
	  implicit val injector = new GuiceBrewjector(Guice.createInjector())
    implicit val converter = StandardBrewverter()
	  val employeeDirector = injector.getInstance(classOf[EmployeeDirector]).right.get
    val node = {shell.evaluate(script1)}.asInstanceOf[groovy.util.Node]
    val typeDesc = ClassTreeGenerator.getTypeDescriptor(classOf[EmployeeDirector])
    Brewsetter.initializeType(typeDesc, employeeDirector, node) match {
      case Left(error) => 
        fail("Could not create type " + error.errorMessage)
      case Right(_) =>
    }
    
    val map = employeeDirector.employees
    expect(10)(map.size)
    val jackman = map.get("JACKMAN").asInstanceOf[FunctionalProgrammer]
    val odersky = employeeDirector.programmer
	  expect("Scala")(jackman.favoriteLanguage)
	  expect(5.01)(map.get("SIMONE").asInstanceOf[Waiter].tips)
	  expect(5.01)(map.get("GOMBERG").asInstanceOf[Waiter].tips)
	  expect(0)(jackman.startCount)
	  assert(odersky != null)
	  expect("Odersky")(odersky.name)
	  expect(5)(odersky.strength)
	  expect("Pizza")(odersky.favoriteLanguage)
    
   
    Brewsetter.startType(typeDesc, employeeDirector) match {
      case Some(error) =>
        fail("Could not start type" + error.errorMessage)
      case None =>
    }
    
    expect(1)(jackman.startCount)
    expect(1)(odersky.startCount)
    
	  
   
   
   
//	  val injector = Guice.createInjector()
   
//	  val employeeDirector = injector.getInstance(classOf[EmployeeDirector])
//    val node = {shell.evaluate(script1)}.asInstanceOf[groovy.util.Node]
//    val tracer = new RecordingTracer()
//    val typeDesc = ClassTreeGenerator.getTypeDescriptor(classOf[EmployeeDirector])
//    println(node)
//    Brewmaster.initialize(employeeDirector, node, typeDesc)(injector.getInstance(_), 
//                                                            shell, tracer)
//    val map = employeeDirector.employees 
//    expect(1)(employeeDirector.initializeCount)
//    expect(10)(map.size)
//    expect("Scala")(map.get("Ben").asInstanceOf[FunctionalProgrammer].favoriteLanguage)
//    expect(5.01)(map.get("Simone").asInstanceOf[Waiter].tips)
//    expect("lolcode")(map.get("FooBarius Maximus").asInstanceOf[FunctionalProgrammer].favoriteLanguage)
//    expect("Pizza")(employeeDirector.programmer.asInstanceOf[FunctionalProgrammer].favoriteLanguage)
//    expect(0)(employeeDirector.programmer.asInstanceOf[FunctionalProgrammer].startCount)
    
	}
}
