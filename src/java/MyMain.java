import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Pattern;

/**
 * My initial solution to the problem. The performance isn't great and the output can be wrong.
 * 
 * @author David Alvarez Fidalgo
 */
public class MyMain {

	public static void main(String[] args) {
		// long t1 = System.currentTimeMillis();
		
		PhoneProblemSolver pps = new PhoneProblemSolver(args.length > 0 ? args[0] : "tests/words.txt");
		List<String> phoneNumbers = loadPhoneNumbers(args.length > 1 ? args[1] : "tests/numbers.txt");

		for (String number : phoneNumbers) {
			List<String> solutions = pps.solve(number);
			for (String solution : solutions) {
				System.out.println(number + ": " + solution);
			}
		}

		// System.out.printf("Time: %d ms\n", System.currentTimeMillis() - t1);
	}

	/**
	 * Loads the phone numbers from a file
	 * @param fileName name of the file
	 * @return list of phone numbers
	 */
	private static List<String> loadPhoneNumbers(String fileName) {
		BufferedReader file = null;
		List<String> phoneNumbers = new ArrayList<>();

		try {
			file = new BufferedReader(new FileReader(fileName));
			while (file.ready()) {
				phoneNumbers.add(file.readLine().trim());
			}
		} catch (FileNotFoundException e) {
			System.out.println("File not found: " + fileName);
		} catch (IOException e) {
			System.out.println("Error reading from file: " + fileName);
		}

		return phoneNumbers;
	}

	/**
	 * Stores a word and its "clean" for. E. g.:
	 * 		Word: O"l
	 * 		Cleaned: ol
	 * 
	 * @author David Alvarez Fidalgo
	 */
	private static class DictionaryWord implements Comparable<DictionaryWord> {

		private static final Pattern REGEX = Pattern.compile("[^a-zA-Z]");

		private String original;
		private String cleaned;

		/**
		 * Creates a new dictionary word
		 * @param word original word
		 */
		public DictionaryWord(String word) {
			this.original = word;
			this.cleaned = null;
		}

		/**
		 * @return original form of the word
		 */
		public String getOriginal() {
			return original;
		}

		/**
		 * @return cleaned for of the word (without double quotes and in lower case)
		 */
		public String getCleaned() {
			if (cleaned == null)
				cleaned = REGEX.matcher(original).replaceAll("").toLowerCase();
			return cleaned;
		}

		@Override
		public int compareTo(MyMain.DictionaryWord o) {
			return this.getCleaned().compareTo(o.getCleaned());
		}
	}

	/**
	 * Implements the solution to the problem
	 * 
	 * @author David Alvarez Fidalgo
	 */
	private static class PhoneProblemSolver {

		private HashMap<Character, TreeSet<DictionaryWord>> dict;
		private List<Integer> phoneNumber;
		private char[][] charsPerDigit;
		private int maxWordLength;
		private int minWordLength;

		/**
		 * Creates an instance of the solver and loads the words from a dictionary file
		 * @param dictionaryFileName name of the file containing the words
		 */
		public PhoneProblemSolver(String dictionaryFileName) {
			charsPerDigit = new char[10][];

			charsPerDigit[0] = new char[1];
			charsPerDigit[0][0] = 'e';

			charsPerDigit[1] = new char[3];
			charsPerDigit[1][0] = 'j';
			charsPerDigit[1][1] = 'n';
			charsPerDigit[1][2] = 'q';

			charsPerDigit[2] = new char[3];
			charsPerDigit[2][0] = 'r';
			charsPerDigit[2][1] = 'w';
			charsPerDigit[2][2] = 'x';

			charsPerDigit[3] = new char[3];
			charsPerDigit[3][0] = 'd';
			charsPerDigit[3][1] = 's';
			charsPerDigit[3][2] = 'y';

			charsPerDigit[4] = new char[2];
			charsPerDigit[4][0] = 'f';
			charsPerDigit[4][1] = 't';

			charsPerDigit[5] = new char[2];
			charsPerDigit[5][0] = 'a';
			charsPerDigit[5][1] = 'm';

			charsPerDigit[6] = new char[3];
			charsPerDigit[6][0] = 'c';
			charsPerDigit[6][1] = 'i';
			charsPerDigit[6][2] = 'v';

			charsPerDigit[7] = new char[3];
			charsPerDigit[7][0] = 'b';
			charsPerDigit[7][1] = 'k';
			charsPerDigit[7][2] = 'u';

			charsPerDigit[8] = new char[3];
			charsPerDigit[8][0] = 'l';
			charsPerDigit[8][1] = 'o';
			charsPerDigit[8][2] = 'p';

			charsPerDigit[9] = new char[3];
			charsPerDigit[9][0] = 'g';
			charsPerDigit[9][1] = 'h';
			charsPerDigit[9][2] = 'z';

			loadDict(dictionaryFileName);
		}

		/**
		 * Solves the phone encoding problem
		 * @param phoneNumberStr string containing the phone number
		 * @return list of solutions
		 */
		public List<String> solve(String phoneNumberStr) {
			loadPhoneNumber(phoneNumberStr);
			List<String> solutions = new ArrayList<>();

			// If the phone number has only one digit and the shortest word has more than one letter,
			// the only solution is the digit itself
			if (phoneNumber.size() == 1 && minWordLength > 1) {
				solutions.add(phoneNumber.get(0).toString());
				return solutions;
			}

			// If the phone number is empty or the phone number has more than one digits but less than the
			// length of the shortest word, there is no solution
			if (phoneNumber.size() == 0 || (phoneNumber.size() > 1 && phoneNumber.size() < minWordLength)) {
				return solutions;
			}

			solveRec(0, new ArrayList<>(), solutions, false);
			return solutions;
		}

		/**
		 * Loads the words from a file
		 * @param dictionaryFileName name of the file containing the words
		 */
		private void loadDict(String dictionaryFileName) {
			BufferedReader file = null;
			
			// We store the words in a hash table containing a tree for each lower case letter
			// We use the "clean" form of the words to compare them, so some words with the same clean
			// form (e. g. O"l and Ol) can get lost because the trees don't store duplicates
			dict = new HashMap<Character, TreeSet<DictionaryWord>>();
			maxWordLength = 0;
			minWordLength = Integer.MAX_VALUE;

			try {
				file = new BufferedReader(new FileReader(dictionaryFileName));
				String line;
				char firstLetter;

				while (file.ready()) {
					line = file.readLine().trim();
					firstLetter = Character.toLowerCase(line.charAt(0));
					if (!dict.containsKey(firstLetter))
						dict.put(firstLetter, new TreeSet<>());
					dict.get(firstLetter).add(new DictionaryWord(line));
					if (line.length() > maxWordLength)
						maxWordLength = line.length();
					if (line.length() < minWordLength)
						minWordLength = line.length();
				}
			} catch (FileNotFoundException e) {
				System.out.println("File not found: " + dictionaryFileName);
			} catch (IOException e) {
				System.out.println("Error reading from file: " + dictionaryFileName);
			}
		}

		/**
		 * Converts the string containing the phone number into a list of integers
		 * @param phoneNumberStr string containing the phone number
		 */
		private void loadPhoneNumber(String phoneNumberStr) {
			phoneNumber = new ArrayList<>();
			for (int i = 0; i < phoneNumberStr.length(); i++) {
				if (Character.isDigit(phoneNumberStr.charAt(i))) {
					phoneNumber.add((int) phoneNumberStr.charAt(i) - 48);
				}
			}
		}

		/**
		 * Recursive method that builds the solution.
		 * We reduce the number of steps needed to encode a phone number by grouping words by its
		 * length. The partial solution is an irregular matrix that holds a sequence of list with
		 * words of the same length. E. g. if 123 translates to both "foo" and "bar" and the number
		 * to encode is 12345, we can group "foo" and "bar", and then encode 45 only one time, 
		 * instead of having to do it twice.
		 * @param digitPos position of the current phone number digit
		 * @param partSolution current partial solution
		 * @param solutions list of completed solutions
		 * @param lastWasDigit whether the last string added to the partial solution was a digit or not
		 */
		private void solveRec(int digitPos, List<List<String>> partSolution, List<String> solutions,
				boolean lastWasDigit) {
			Map<Integer, List<String>> words = findWords(digitPos);
			boolean wordsFound = !words.isEmpty();

			// If no word was found, we add the digit to the solution
			if (!wordsFound) {
				// We can't have two consecutive digits in the solution
				if (lastWasDigit)
					return;
				List<String> wordList = new ArrayList<>();
				wordList.add(Integer.toString(phoneNumber.get(digitPos)));
				words.put(1, wordList);
			}

			for (Map.Entry<Integer, List<String>> entry : words.entrySet()) {
				partSolution.add(entry.getValue());
				if (digitPos + entry.getKey() == phoneNumber.size()) {
					solutions.addAll(createSolutionList(partSolution));
				} else {
					solveRec(digitPos + entry.getKey(), partSolution, solutions, !wordsFound);
				}
				partSolution.remove(partSolution.size() - 1);
			}

		}

		/**
		 * Transforms an irregular matrix containing the encodings of a phone number into a list
		 * of solutions
		 * @param solutionMatrix irregular matrix containing the encodings
		 * @return list of solutions
		 */
		private List<String> createSolutionList(List<List<String>> solutionMatrix) {
			List<String> solutionsList = new ArrayList<>();
			createSolutionListRec(0, new ArrayList<>(), solutionMatrix, solutionsList);
			return solutionsList;
		}

		/**
		 * Recursively transforms an irregular matrix containing the encodings of a phone number into a list
		 * of solutions 
		 * @param i next x index in the matrix
		 * @param partSolution current partial solution
		 * @param solutionMatrix irregular matrix containing the encodings of a phone number
		 * @param solutionsList list of solutions
		 */
		private void createSolutionListRec(int i, List<String> partSolution, List<List<String>> solutionMatrix,
				List<String> solutionsList) {
			if (i == solutionMatrix.size()) {
				solutionsList.add(joinWordList(partSolution));
				return;
			}

			for (String word : solutionMatrix.get(i)) {
				partSolution.add(word);
				createSolutionListRec(i + 1, partSolution, solutionMatrix, solutionsList);
				partSolution.remove(partSolution.size() - 1);
			}
		}

		/**
		 * Joins a list of words leaving spaces between them
		 * @param wordList list of words
		 * @return string containing the list of words joined with spaces between them
		 */
		private String joinWordList(List<String> wordList) {
			StringBuilder sb = new StringBuilder(wordList.get(0));
			for (int i = 1; i < wordList.size(); i++) {
				sb.append(" " + wordList.get(i));
			}
			return sb.toString();
		}

		/**
		 * Finds all the words that can be formed starting from a digit of the phone number
		 * @param digitPos digit of the phone number
		 * @return list of words found, grouped by length
		 */
		private Map<Integer, List<String>> findWords(int digitPos) {
			Map<Integer, List<String>> words = new HashMap<>();
			findWordsRec(digitPos, 0, new char[maxWordLength], words);
			return words;
		}

		/**
		 * Recursively finds all the words that can be formed starting from a digit of the phone number
		 * @param digitPos digit of the phone number
		 * @param letterPos position of the next letter to be added to the current partial word
		 * @param partWord current partial word
		 * @param words list of words found
		 */
		private void findWordsRec(int digitPos, int letterPos, char[] partWord, Map<Integer, List<String>> words) {
			for (int i = 0; i < charsPerDigit[phoneNumber.get(digitPos)].length; i++) {
				partWord[letterPos] = charsPerDigit[phoneNumber.get(digitPos)][i];
				if (checkPartWord(letterPos, partWord, words) && digitPos + 1 < phoneNumber.size()
						&& letterPos + 1 < partWord.length) {
					findWordsRec(digitPos + 1, letterPos + 1, partWord, words);
				}
				partWord[letterPos] = 0;
			}
		}

		/**
		 * Checks if a partial word is equal to some word in the dictionary and if there is a word 
		 * in the dictionary that starts like the partial word
		 * @param letterPos position of the last letter in the current partial word
		 * @param partWord current partial word
		 * @param words list of words found
		 * @return true if we can form a dictionary word with the current partial word or false if we can't
		 */
		private boolean checkPartWord(int letterPos, char[] partWord, Map<Integer, List<String>> words) {
			String word = new String(partWord).trim();

			if (dict.containsKey(partWord[0])) {
				DictionaryWord firstEqualOrGreater = dict.get(partWord[0]).ceiling(new DictionaryWord(word));
				String firstEqualOrGreaterLowerCleaned = firstEqualOrGreater == null ? null
						: firstEqualOrGreater.getCleaned();
				if (firstEqualOrGreaterLowerCleaned == null
						|| !(word.length() <= firstEqualOrGreaterLowerCleaned.length()
								&& word.substring(0, letterPos + 1)
										.equals(firstEqualOrGreaterLowerCleaned.substring(0, letterPos + 1))))
					return false;
				if (firstEqualOrGreaterLowerCleaned.equals(word)) {
					if (!words.containsKey(word.length()))
						words.put(word.length(), new ArrayList<>());
					words.get(word.length()).add(firstEqualOrGreater.getOriginal());
				}
				return true;
			}

			return false;
		}

	}

}
