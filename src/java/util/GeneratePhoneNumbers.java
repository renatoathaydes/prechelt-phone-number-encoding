package util;

import java.util.Random;
import java.util.stream.IntStream;

/**
 * Prints 1000 random phone numbers.
 * <p>
 * By default, 1000 numbers are generated. To modify that, pass the desired count as an argument.
 * <p>
 * The second argument can be used to set a limit on the length of
 * each number (it's 50 by default).
 *
 * @author Renato Athaydes
 */
public class GeneratePhoneNumbers {
    public static void main( String[] args ) {
        var count = args.length == 0 ? 1000 : Integer.parseInt( args[ 0 ] );
        var maxPhoneLength = args.length > 1 ? Integer.parseInt( args[ 1 ] ) : 50;

        /*
        Example phone numbers used in study:
            112
            5624-82
            4824
            0721/608-4067
            10/783--5
            1078-913-5
            381482
            04824
         */
        char[] inputChars = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '/', '-' };

        var random = new Random();

        // generate 1000 numbers
        IntStream.range( 0, count ).mapToObj( ignore ->
                nextPhoneNumber( random, inputChars, maxPhoneLength )
        ).forEach( System.out::println );
    }

    private static String nextPhoneNumber( Random random, char[] inputChars, int maxPhoneLength ) {
        var phoneLength = random.nextInt( maxPhoneLength ) + 1;
        var builder = new StringBuilder( phoneLength );
        while ( true ) {
            for ( var i = 0; i < phoneLength; i++ ) {
                builder.append( inputChars[ random.nextInt( inputChars.length ) ] );
            }
            var phone = builder.toString();

            if ( !phone.replaceAll( "[-/]", "" ).isEmpty() ) {
                return phone;
            }
        }
    }
}
