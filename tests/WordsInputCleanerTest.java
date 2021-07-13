import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class WordsInputCleanerTest {
    static Stream<Arguments> canCleanWordsExamples() {
        return Stream.of(
                Arguments.of( "", "" ),
                Arguments.of( "a", "a" ),
                Arguments.of( "a23-b\tc//", "abc" ),
                Arguments.of( "a2b-4\t\"c\"  ", "abc" )
        );
    }

    @ParameterizedTest
    @MethodSource( "canCleanWordsExamples" )
    public void canCleanWords( String word, String expected ) {
        assertEquals( expected, WordsInputCleaner.clean( word ) );
    }

}
