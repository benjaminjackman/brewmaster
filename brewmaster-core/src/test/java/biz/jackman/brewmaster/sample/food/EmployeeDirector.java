package biz.jackman.brewmaster.sample.food;


import java.util.LinkedHashMap;
import java.util.Map;

import biz.jackman.brewmaster.annotations.Child;
import biz.jackman.brewmaster.annotations.Mapped;
import biz.jackman.brewmaster.annotations.Tag;

import com.google.inject.Singleton;

@Singleton
@Tag("Employees")
public class EmployeeDirector {
	@Mapped(key = String.class, base = AbstractEmployee.class, derived = {
			Cook.class, biz.jackman.brewmaster.sample.food.Waiter.class, biz.jackman.brewmaster.sample.food.FunctionalProgrammer.class})
	final Map<String, AbstractEmployee> employees = new LinkedHashMap<String, AbstractEmployee>();
	
	@Child
	final biz.jackman.brewmaster.sample.food.FunctionalProgrammer programmer = null;

	int initializeCount = 0;
	
	
}
