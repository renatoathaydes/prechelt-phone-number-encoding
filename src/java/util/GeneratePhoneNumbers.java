package util;

import java.util.Random;
import java.util.stream.IntStream;

/**
 * Prints 1000 random phone numbers.
 * <p>
 * By default, 1000 numbers are generated. To modify that, pass the desired count as an argument.
 * <p>
 * The second argument can be "true" to allow phone numbers consisting of only non-digits (empty after clean).
 *
 * @author Renato Athaydes
 */
public class GeneratePhoneNumbers {
    public static void main( String[] args ) {
        var count = args.length == 0 ? 1000 : Integer.parseInt( args[ 0 ] );
        var allowEmpty = args.length > 1 ? Boolean.parseBoolean( args[ 1 ] ) : false;

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
                nextPhoneNumber( random, inputChars, allowEmpty )
        ).forEach( System.out::println );
    }

    private static String nextPhoneNumber( Random random, char[] inputChars, boolean allowEmpty ) {
        var phoneLength = random.nextInt( 50 ) + 1;
        var builder = new StringBuilder( phoneLength );
        while ( true ) {
            for ( var i = 0; i < phoneLength; i++ ) {
                builder.append( inputChars[ random.nextInt( inputChars.length ) ] );
            }
            var phone = builder.toString();

            if ( allowEmpty || !phone.replaceAll( "[-/]", "" ).isEmpty() ) {
                return phone;
            }
        }
    }
}
