package biz.jackman.brewmaster.sample.food;


import biz.jackman.brewmaster.annotations.Attribute;
import biz.jackman.brewmaster.annotations.InheritAttributes;

@InheritAttributes
public class Cook extends AbstractEmployee {
	
	@Attribute
	String catchphrase;
}
