/* Name:

   UID:

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    // double eval(); 	                       // Problem 1a
    // List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for arithmetic instructions
interface Instr {
}

class Push implements Instr {
    protected double val;

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }

}

class Calculate implements Instr {
    protected Op op;

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }    
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    // public double execute() {}  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
	 //    // a test for Problem 1a
		// Exp exp =
	 //    	new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
		//     	  	  Op.TIMES,
		//       	  new Num(3.0));
		// assert(exp.eval() == 9.0);

		// // a test for Problem 1b
		// List<Instr> is = new LinkedList<Instr>();
		// is.add(new Push(1.0));
		// is.add(new Push(2.0));
		// is.add(new Calculate(Op.PLUS));
		// is.add(new Push(3.0));
		// is.add(new Calculate(Op.TIMES));
		// Instrs instrs = new Instrs(is);
		// assert(instrs.execute() == 9.0);

		// // a test for Problem 1c
		// assert(exp.compile().equals(is));
    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
//     int size();
//     boolean contains(String s);
//     void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    protected SNode head;
}

// a type for the nodes of the linked list
interface SNode {
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;
}

