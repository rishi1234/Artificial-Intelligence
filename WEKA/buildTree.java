import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;

import weka.classifiers.trees.J48;
import weka.core.Instances;
import weka.gui.treevisualizer.PlaceNode2;
import weka.gui.treevisualizer.TreeVisualizer;

public class buildTree extends JFrame {
	static String s;
	private static JFileChooser c = new JFileChooser();
	private static String file_path;

	public static void main(String args[]) throws Exception {

		JFrame n = new JFrame();
		n.setFont(new Font("Serif", Font.BOLD, 15));
		n.setBackground(Color.BLACK);
		n.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		n.setSize(600, 300);
		n.setLocation(200, 200);
		n.setTitle("WEKA ");
		n.getContentPane().setLayout(new FlowLayout());

		JButton Browse = new JButton("Browse");
		Browse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					int a = c.showOpenDialog(null);
					if (a == JFileChooser.APPROVE_OPTION) {
						file_path = c.getSelectedFile().getAbsolutePath();
						start(file_path);
					}
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});
		n.add(Browse);
		n.setVisible(true);

	}

	public static void start(String s) throws Exception {
		J48 cls = new J48();
		Instances data = new Instances(new BufferedReader(new FileReader(s)));
		data.setClassIndex(data.numAttributes() - 1);
		cls.buildClassifier(data);
		final JFrame f = new JFrame("Tree");
		f.setSize(600, 500);
		f.getContentPane().setLayout(new BorderLayout());
		TreeVisualizer t = new TreeVisualizer(null, cls.graph(),
				new PlaceNode2());
		f.getContentPane().add(t, BorderLayout.CENTER);
		f.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				f.dispose();
			}
		});

		f.setVisible(true);
		t.fitToScreen();
	}
}