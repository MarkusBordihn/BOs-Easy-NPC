/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.commands.selector;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.context.StringRange;
import com.mojang.brigadier.suggestion.Suggestion;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.suggestion.EasyNPCSuggestions;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import java.util.stream.Stream;
import net.minecraft.commands.arguments.selector.EntitySelectorParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCSelectorParser extends EntitySelectorParser {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPCSelectorParser(StringReader stringReader) {
    this(stringReader, true);
  }

  public EasyNPCSelectorParser(StringReader stringReader, boolean allowSelectors) {
    super(stringReader, allowSelectors);
  }

  @Override
  public CompletableFuture<Suggestions> fillSuggestions(
      SuggestionsBuilder suggestionsBuilder, Consumer<SuggestionsBuilder> consumer) {

    // Define suggestion string range
    StringRange stringRange =
        StringRange.between(suggestionsBuilder.getStart(), suggestionsBuilder.getInput().length());

    // Filter out player selectors like @p, @a, @r and @s.
    List<Suggestion> suggestionsList = new ArrayList<>();
    try {
      CompletableFuture<Suggestions> suggestionsOriginal =
          super.fillSuggestions(suggestionsBuilder, consumer);
      List<Suggestion> originalSuggestionsList = suggestionsOriginal.get().getList();
      stringRange = suggestionsOriginal.get().getRange();
      originalSuggestionsList.stream()
          .filter(suggestion -> !suggestion.getText().matches("@[pars]"))
          .forEach(
              suggestion -> {
                if (suggestion.getText().startsWith("@")) {
                  suggestionsList.add(suggestion);
                } else {
                  suggestionsList.add(0, suggestion);
                }
              });
    } catch (InterruptedException | ExecutionException e) {
      log.error("Failed to get suggestions: {}", e.getMessage());
    }

    // Add known UUIDs to suggestions
    String argument = suggestionsBuilder.getRemainingLowerCase();
    if (argument != null && !argument.isEmpty() && !argument.startsWith("@")) {
      Stream<String> possibleUUIDs = EasyNPCSuggestions.suggestUUID(argument);
      if (possibleUUIDs != null) {
        possibleUUIDs.forEach(
            uuid -> {
              for (Suggestion suggestion : suggestionsList) {
                if (suggestion.getText().equals(uuid)) {
                  return;
                }
              }
              suggestionsList.add(
                  new Suggestion(
                      StringRange.between(
                          suggestionsBuilder.getStart(), suggestionsBuilder.getInput().length()),
                      uuid));
            });
      }
    }

    // Build new suggestions with the combined list.
    CompletableFuture<Suggestions> suggestions =
        CompletableFuture.completedFuture(new Suggestions(stringRange, suggestionsList));
    this.setSuggestions((suggestionsBuilder1, consumer1) -> suggestions);
    return suggestions;
  }
}
