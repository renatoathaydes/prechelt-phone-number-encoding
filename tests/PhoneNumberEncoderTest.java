import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class PhoneNumberEncoderTest {

    static final Set<Item> WORDS = Set.of(
            "ad",
            "Bo\"",
            "bo\"s",
            "da",
            "fort",
            "je",
            "mir",
            "Mix",
            "neu",
            "o\"d",
            "so",
            "Tor",
            "Torf"
    ).stream().map( w -> new Item( w, WordsInputCleaner.clean( w ) ) ).collect( Collectors.toSet() );

    static Stream<Arguments> canEncodePhoneNumbersExamples() {
        return Stream.of(
                Arguments.of( new Item( "5624-82", "562482" ), Set.of( "mir Tor", "Mix Tor" ) ),
                Arguments.of( new Item( "4824" ), Set.of( "Torf", "fort", "Tor 4" ) ),
                Arguments.of( new Item( "10/783--5", "107835" ), Set.of( "neu o\"d 5", "je bo\"s 5", "je Bo\" da" ) ),
                Arguments.of( new Item( "381482" ), Set.of( "so 1 Tor" ) ),
                Arguments.of( new Item( "04824" ), Set.of( "0 Torf", "0 fort", "0 Tor 4" ) ),
                Arguments.of( new Item( "/5//-39-", "539" ), Set.of( "ad 9" ) )
        );
    }

    static final PhoneNumberEncoder encoder = new PhoneNumberEncoder( WORDS.stream() );

    // the encode2 does not use Item as input as it cleans the numbers itself
    static final PhoneNumberEncoder2 encoder2 = new PhoneNumberEncoder2( WORDS.stream().map( i -> i.original() ) );

    @ParameterizedTest
    @MethodSource( "canEncodePhoneNumbersExamples" )
    public void canEncodePhoneNumbers( Item phone, Set<String> possibleEncodings ) {
        var expected = possibleEncodings.stream()
                .map( possibility -> new Item( phone.original(), possibility ) )
                .collect( Collectors.toSet() );

        assertEquals( expected, encode( phone ) );
    }

    @ParameterizedTest
    @MethodSource( "canEncodePhoneNumbersExamples" )
    public void canEncodePhoneNumbers2( Item phone, Set<String> possibleEncodings ) {
        var expected = possibleEncodings.stream()
                .map( possibility -> new Item( phone.original(), possibility ) )
                .collect( Collectors.toSet() );

        assertEquals( expected, encode2( phone.original() ) );
    }

    private static Set<Item> encode( Item phone ) {
        var result = new HashSet<Item>();
        encoder.encode( phone, result::add );
        return result;
    }

    private static Set<Item> encode2( String phone ) {
        var result = new HashSet<Item>();
        encoder2.encode( phone, ( num, solution ) -> result.add( new Item( num, solution ) ) );
        return result;
    }

}
