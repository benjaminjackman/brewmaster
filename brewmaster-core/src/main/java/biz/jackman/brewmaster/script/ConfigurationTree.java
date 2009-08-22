package biz.jackman.brewmaster.script;

import groovy.util.Node;

public class ConfigurationTree {
	final Node root;

	public Node getRoot() {
		return root;
	}

	public ConfigurationTree(Node root) {
		super();
		this.root = root;
	}

}
