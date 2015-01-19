import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.util.Scanner;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.trees.J48;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

public class loadTree1 {

	public static String Str1, Str2, Str3, Str4, Str5, Str6, s1 = null,
			s2 = null;
	public static int pr1, pr2, pr3, pr4, pr5, pr6;
	static String s;
	public static JFileChooser c = new JFileChooser();
	public static String file_path, file_path1;
	static Classifier cls;
	static Instances data = null;
	public static int attrib;
	static StringBuilder sb = new StringBuilder();
	static int l = 0;

	public static void main(String[] args) throws Exception {
		start();
	}

	private static void start() throws Exception {
		System.out.println("Menu");
		System.out.println("----------");
		System.out
				.println("1. Learn a decision tree or load an existing tree.");
		System.out.println("2. Testing accuracy of the decision tree.");
		System.out.println("3. Applying the decision tree to new cases.");
		System.out.println("4. Quit");
		InputStreamReader In = new InputStreamReader(System.in);
		BufferedReader br = new BufferedReader(In);
		System.out.println("Enter a value:");
		Str1 = br.readLine();
		pr1 = Integer.parseInt(Str1);
		if (pr1 == 1) {
			first();
		}
		if (pr1 == 2) {
			second();
		}
		if (pr1 == 3) {
			third();
		}
		if (pr1 == 4) {
			fourth();
		}

	}

	private static void fourth() throws Exception {
		InputStreamReader In = new InputStreamReader(System.in);
		BufferedReader br = new BufferedReader(In);
		System.out.println("Do you really want to exit:");
		System.out.println("enter 1 for yes");
		System.out.println("enter 2 for no");
		Str1 = br.readLine();
		pr1 = Integer.parseInt(Str1);
		if (pr1 == 1) {

			System.exit(0);
		} else {
			start();
		}
	}

	private static void third() throws Exception {
		InputStreamReader In4 = new InputStreamReader(System.in);
		BufferedReader br4 = new BufferedReader(In4);
		System.out.println("1. Enter a new case interactively");
		System.out.println("2. Quit");
		Str4 = br4.readLine();
		pr4 = Integer.parseInt(Str4);
		if (pr4 == 1) {
			third_1();
		} else if (pr4 == 2) {
			start();
		} else {
			third();
		}

	}

	private static void second() {
		second_1();

	}

	private static void first() throws IOException {
		first_1();

	}

	private static void first_1() throws IOException {
		InputStreamReader In1 = new InputStreamReader(System.in);
		BufferedReader br1 = new BufferedReader(In1);
		System.out
				.println("Do you want to load tree or load a attribute file:");
		System.out.println("1. Load Tree");
		System.out.println("2. Load Attribute file");
		Str2 = br1.readLine();
		pr2 = Integer.parseInt(Str2);
		if (pr2 == 1) {
			JFrame n = new JFrame();
			n.setFont(new Font("Serif", Font.BOLD, 15));
			n.setBackground(Color.BLACK);
			n.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			n.setSize(600, 300);
			n.setLocation(200, 200);
			n.setTitle("WEKA Load Tree");
			n.getContentPane().setLayout(new FlowLayout());

			JButton Browse = new JButton("Browse");
			Browse.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					try {
						int a = c.showOpenDialog(null);
						if (a == JFileChooser.APPROVE_OPTION) {
							file_path = c.getSelectedFile().getAbsolutePath();
							file_path1 = c.getSelectedFile().getAbsolutePath();
							// System.out.println("file_path" + file_path);
							start_1(file_path);
						}
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
			});
			n.add(Browse);
			n.setVisible(true);
			l = 1;

		} else if (pr2 == 2) {
			JFrame n = new JFrame();
			n.setFont(new Font("Serif", Font.BOLD, 15));
			n.setBackground(Color.BLACK);
			n.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			n.setSize(600, 300);
			n.setLocation(200, 200);
			n.setTitle("WEKA Load Attributes and Data");
			n.getContentPane().setLayout(new FlowLayout());

			JButton Browse = new JButton("Browse");
			Browse.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					try {
						int a = c.showOpenDialog(null);
						if (a == JFileChooser.APPROVE_OPTION) {
							file_path = c.getSelectedFile().getAbsolutePath();
							start_2(file_path);
						}
					} catch (Exception e1) {
						e1.printStackTrace();
					}
				}

			});
			n.add(Browse);
			n.setVisible(true);
			l = 2;

		} else {
			first();
		}

	}

	private static void start_1(String file_path) throws Exception {
		cls = new J48();
		ObjectInputStream a = new ObjectInputStream(new FileInputStream(
				file_path));
		cls = (Classifier) a.readObject();
		a.close();
		System.out.println("Classifier :" + cls);
		start();
	}

	private static void start_2(String file_path) throws Exception {
		cls = new J48();
		Instances data = new Instances(new BufferedReader(new FileReader(
				file_path)));
		System.out.println(data);
		start();
	}

	private static void second_1() {
		JFrame n = new JFrame();
		n.setFont(new Font("Serif", Font.BOLD, 15));
		n.setBackground(Color.BLACK);
		n.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		n.setSize(600, 300);
		n.setLocation(200, 200);
		n.setTitle("WEKA Load Testing File");
		n.getContentPane().setLayout(new FlowLayout());

		JButton Browse = new JButton("Browse");
		Browse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					int a = c.showOpenDialog(null);
					if (a == JFileChooser.APPROVE_OPTION) {
						file_path = c.getSelectedFile().getAbsolutePath();
						start_3(file_path);
					}
				} catch (Exception e1) {

					e1.printStackTrace();
				}
			}

		});
		n.add(Browse);
		n.setVisible(true);
	}

	private static void start_3(String file_path) throws Exception {
		try {
			data = new Instances(new BufferedReader(new FileReader(file_path)));
			System.out.println(data.numAttributes());
			attrib = data.numAttributes();

			// System.out.println("Classifier :" + cls);
			// System.out.println("Data :" + data);
			data.setClassIndex(data.numAttributes() - 1);
			cls.buildClassifier(data);
			Evaluation eval = new Evaluation(data);
			eval.evaluateModel(cls, data);
			System.out.println(eval.toSummaryString("\nResult is \n======\n",
					true));
			System.out.println((eval.toMatrixString()));
		} catch (Exception e) {
			System.out.println("Load the Classifier");
		}
		start();
	}

	private static void third_1() throws Exception {
		try {
			int j;
			System.out.println("Enter the path of the training data (.arff):");
			Scanner a = new Scanner(System.in);
			String s1 = a.nextLine();
			Instances train_data = new Instances(new BufferedReader(
					new FileReader(s1)));
			train_data.setClassIndex(train_data.numAttributes() - 1);

			System.out.println("Enter the path of the decision tree (.model)");
			Scanner b = new Scanner(System.in);
			String s2 = b.nextLine();
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(
					s2));
			Classifier cls = (Classifier) ois.readObject();
			ois.close();

			int attrb = train_data.numAttributes();
			Instance tst = new Instance(attrb);
			tst.setDataset(train_data);

			System.out.println("How many cases do you want to enter:");
			Scanner d = new Scanner(System.in);
			String d1 = d.nextLine();
			int k = Integer.parseInt(d1);

			for (int i = 0; i < k; i++) {
				for (int m = 0; m < attrb; m++) {
					if (train_data.classIndex() != m) {
						Attribute attr = train_data.attribute(m);
						System.out.print("Enter the value for " + attr.name()
								+ ": ");
						if (attr.isNominal()) {
							tst.setValue(m, a.next());
						} else {
							tst.setValue(m, Double.parseDouble(a.next()));
						}
					}
				}
			}
			System.out.println(cls);
			double result = cls.classifyInstance(tst);
			System.out.println("Decision is : " + result);

		} catch (Exception e) {
			System.out
					.println("Error: Value not defined for given nominal attribute!");
		}
		start();
	}
}
