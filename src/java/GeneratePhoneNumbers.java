import java.util.Random;
import java.util.stream.IntStream;

/**
 * Prints 1000 random phone numbers.
 * <p>
 * By default, 1000 numbers are generated. To modify that, pass the desired count as an argument.
 */
public class GeneratePhoneNumbers {
    public static void main( String[] args ) {
        var count = args.length == 0 ? 1000 : Integer.parseInt( args[ 0 ] );

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
                nextPhoneNumber( random, inputChars )
        ).forEach( System.out::println );
    }

    private static String nextPhoneNumber( Random random, char[] inputChars ) {
        var phoneLength = random.nextInt( 50 ) + 1;
        var builder = new StringBuilder( phoneLength );
        while ( true ) {
            for ( var i = 0; i < phoneLength; i++ ) {
                builder.append( inputChars[ random.nextInt( inputChars.length ) ] );
            }
            var phone = builder.toString();

            // do not use empty phone numbers
            if ( !PhoneNumberCleaner.clean( phone ).isEmpty() ) {
                return phone;
            }
        }
    }
}
