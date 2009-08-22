package biz.jackman.brewmaster.script;

import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import groovy.util.Node;

import java.io.File;
import java.io.IOException;

import org.codehaus.groovy.control.CompilationFailedException;


/**
 * This class is used for the loading of configuration files
 * 
 * @author bjackman
 * 
 */
public class BrewmasterScriptEngine {
	public abstract static class ShellDescriptor {
		//The name of the setting that holds the NodeBuilder
		//typically config
		public final String rootName;
		//This class defines all the bindings needed by the script
		public Binding getBinding() {
			return new Binding();
		}
		
		public ShellDescriptor(String rootName) {
			super();
			this.rootName = rootName;
		}
	}
	
	public static Node getRoot(final GroovyShell shell, final String filename, final ShellDescriptor descriptor) {
		try {
			shell.evaluate(new File(filename));
			final Node root = (Node) shell.getVariable(descriptor.rootName);
			return root;
		} catch (CompilationFailedException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	
}
