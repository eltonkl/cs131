import java.io.*;

class Tester {
    public static void main(String[] args) throws FileNotFoundException, IOException {
        PPMImage img = new PPMImage("florence.ppm");
        PPMImage negated = img.negate();
        negated.toFile("florenceneg.ppm");
        PPMImage greyscale = img.greyscale();
        greyscale.toFile("florencegs.ppm");
    }
}
