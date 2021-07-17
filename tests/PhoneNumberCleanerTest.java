import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

public class PhoneNumberCleanerTest {
    static Stream<Arguments> canCleanupPhoneNumbersExamples() {
        return Stream.of(
                Arguments.of( "0", "0" ),
                Arguments.of( "123", "123" ),
                Arguments.of( "1-2-3", "123" ),
                Arguments.of( "1/3", "13" ),
                Arguments.of( "//1--3//", "13" ),
                Arguments.of( "/5//-39-", "539" )
        );
    }

    @ParameterizedTest
    @MethodSource( "canCleanupPhoneNumbersExamples" )
    public void canCleanupPhoneNumbers( String phone, String expected ) {
        Assertions.assertEquals( expected, PhoneNumberCleaner.clean( phone ) );
    }

}
