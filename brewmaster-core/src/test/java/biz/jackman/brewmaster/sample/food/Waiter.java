package biz.jackman.brewmaster.sample.food;


import biz.jackman.brewmaster.annotations.Attribute;
import biz.jackman.brewmaster.annotations.DefaultValue;
import biz.jackman.brewmaster.annotations.InheritAttributes;

@InheritAttributes
public class Waiter extends AbstractEmployee {

	@Attribute()
	@DefaultValue("0.0d")
	double tips = 0.0d;
}
