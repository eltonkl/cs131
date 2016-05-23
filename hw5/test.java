import java.io.*;

class Tester {
    public static void main(String[] args) throws FileNotFoundException, IOException {
        PPMImage img = new PPMImage("florence.ppm");
        
        long time0 = System.nanoTime();
        PPMImage negated = img.negate();
        long time1 = System.nanoTime();
        System.out.println("Negation:  " + (time1 - time0)/1000000000.);
        negated.toFile("florenceneg.ppm");

        negated.negate().toFile("florencenegneg.ppm");

        time0 = System.nanoTime();
        PPMImage greyscale = img.greyscale();
        time1 = System.nanoTime();
        System.out.println("Greyscale: " + (time1 - time0)/1000000000.);
        greyscale.toFile("florencegs.ppm");

        time0 = System.nanoTime();
        PPMImage mirror = img.mirrorImage();
        time1 = System.nanoTime();
        System.out.println("Mirror:    " + (time1 - time0)/1000000000.);
        mirror.toFile("florencemi.ppm");
        mirror.mirrorImage().toFile("florencemimi.ppm");

        time0 = System.nanoTime();
        PPMImage mirror2 = img.mirrorImage2();
        time1 = System.nanoTime();
        System.out.println("Mirror2:   " + (time1 - time0)/1000000000.);
        mirror2.toFile("florencemi2.ppm");

        time0 = System.nanoTime();
        PPMImage gb1 = img.gaussianBlur(60, 2);
        time1 = System.nanoTime();
        System.out.println("GB1:       " + (time1 - time0)/1000000000.);
        gb1.toFile("florencegb1.ppm");
    }
}
