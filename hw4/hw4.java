/*  Name:

    UID:

    Others With Whom I Discussed Things:

    Other Resources I Consulted:
    https://docs.oracle.com/javase/tutorial/java/javaOO/constructors.html
    https://docs.oracle.com/javase/7/docs/api/java/util/Stack.html
    http://docs.oracle.com/javase/tutorial/java/generics/restrictions.html
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }

    public Num(double val) { this.val = val; }
    public double eval() { return val; }

    public List<Instr> compile() {
        List<Instr> list = new LinkedList<Instr>();
        list.add(new Push(val));
        return list;
    }
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

    public BinOp(Exp left, Op op, Exp right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }

    public double eval() {
        return op.calculate(left.eval(), right.eval());
    }

    public List<Instr> compile() {
        List<Instr> ops = new LinkedList<Instr>();
        ops.addAll(left.compile());
        ops.addAll(right.compile());
        ops.add(new Calculate(op));
        return ops;
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
    void execute(Stack<Double> stack);
}

class Push implements Instr {
    protected double val;

    public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }

    public Push(double val) { this.val = val; }
    public void execute(Stack<Double> stack) { stack.push(val); }
}

class Calculate implements Instr {
    protected Op op;

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }

    public Calculate(Op op) { this.op = op; }

    public void execute(Stack<Double> stack) {
        double val1 = stack.pop();
        double val2 = stack.pop();
        stack.push(op.calculate(val2, val1));
    }
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    public double execute() {
        Stack<Double> stack = new Stack<Double>();
        for (Instr i : instrs) {
            i.execute(stack);
        }
        return stack.peek();
    }  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
        // a test for Problem 1a
        Exp exp =
            new BinOp(new BinOp(new Num(4.0), Op.PLUS, new Num(2.0)),
                    Op.DIVIDE,
                    new Num(3.0));
        assert(exp.eval() == 2.0);

	// a test for Problem 1b
	List<Instr> is = new LinkedList<Instr>();
	is.add(new Push(4.0));
	is.add(new Push(2.0));
	is.add(new Calculate(Op.PLUS));
	is.add(new Push(3.0));
	is.add(new Calculate(Op.DIVIDE));
	Instrs instrs = new Instrs(is);
	assert(instrs.execute() == 2.0);

	// a test for Problem 1c
	assert(exp.compile().equals(is));

        // Problem 2
        StringSet s = new ListStringSet();
        assert(s.size() == 0);
        assert(!s.contains(""));
        s.add("x");
        assert(s.contains("x"));
        assert(s.size() == 1);
        s.add("x");
        assert(s.contains("x"));
        assert(s.size() == 1);


        Set<String> t = new ListSet<String>((s1, s2) -> s2.compareTo(s1));
        assert(t.size() == 0);
        assert(!t.contains(""));
        t.add("x");
        assert(t.contains("x"));
        assert(t.size() == 1);
        t.add("x");
        assert(t.contains("x"));
        assert(t.size() == 1);
    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
     int size();
     boolean contains(String s);
     void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    protected SNode head;

    public ListStringSet() { head = new SEmpty(); }

    public int size() {
        int size = 0;
        SNode node = head;
        while (!node.isEmpty()) {
            size += 1;
            node = node.getNext();
        }
        return size;
    }

    public boolean contains(String s) {
        SNode node = head;
        while (!node.isEmpty()) {
            if (node.compareTo(s) == 0)
                return true;
            node = node.getNext();
        }
        return false;
    }

    public void add(String s) {
        head = head.add(s);
    }
}

// a type for the nodes of the linked list
interface SNode {
    boolean isEmpty();
    SNode getNext();
    int compareTo(String s);
    SNode add(String s);
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    public boolean isEmpty() { return true; }
    public SNode getNext() { return new SEmpty(); }
    public int compareTo(String s) { return -1; }
    public SNode add(String s) { return new SElement(s); }
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;

    public SElement(String s) { elem = s; next = new SEmpty(); }

    public boolean isEmpty() { return false; }
    public SNode getNext() { return next; }
    public int compareTo(String s) { return elem.compareTo(s); }
    public SNode add(String s) {
        if (elem.compareTo(s) > 0) {
            SElement next = new SElement(this.elem);
            this.elem = s;
            next.next = this.next;
            this.next = next;
        }
        return this;
    }
}

interface Set<T> {
    int size();
    boolean contains(T t);
    void add(T t);
}

class ListSet<T> implements Set<T> {
    protected Node<T> head;
    protected Comparator<T> comparator;

    public ListSet(Comparator<T> comparator) {
        head = new Empty<T>();
        this.comparator = comparator;
    }

    public int size() {
        int size = 0;
        Node<T> node = head;
        while (!node.isEmpty()) {
            size += 1;
            node = node.getNext();
        }
        return size;
    }

    public boolean contains(T t) {
        Node<T> node = head;
        while (!node.isEmpty()) {
            if (node.compareToWithComparator(t, comparator) == 0)
                return true;
            node = node.getNext();
        }
        return false;
    }

    public void add(T t) {
        head = head.add(t, comparator);
    }
}

interface Node<T> {
    boolean isEmpty();
    Node<T> getNext();
    int compareToWithComparator(T t, Comparator<T> comparator);
    Node<T> add(T t, Comparator<T> comparator);
}

class Empty<T> implements Node<T> {
    public boolean isEmpty() { return true; }
    public Node<T> getNext() { return new Empty<T>(); }
    public int compareToWithComparator(T t, Comparator<T> comparator) { return -1; }
    public Node<T> add(T t, Comparator<T> comparator) { return new Element<T>(t); }
}

class Element<T> implements Node<T> {
    protected T elem;
    protected Node<T> next;

    public Element(T t) { elem = t; next = new Empty<T>(); }

    public boolean isEmpty() { return false; }
    public Node<T> getNext() { return next; }
    public int compareToWithComparator(T t, Comparator<T> comparator) {
        return comparator.compare(elem, t);
    }
    public Node<T> add(T t, Comparator<T> comparator) {
        if (comparator.compare(elem, t) > 0) {
            Element<T> next = new Element<T>(this.elem);
            this.elem = t;
            next.next = this.next;
            this.next = next;
        }
        return this;
    }
}
