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

package de.markusbordihn.easynpc.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import com.mojang.datafixers.util.Pair;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataManager;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.selector.EntitySelector;
import net.minecraft.network.chat.Component;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogArgument implements ArgumentType<Pair<UUID, String>> {

  public static final SimpleCommandExceptionType ERROR_INVALID_UUID =
      new SimpleCommandExceptionType(Component.translatable("argument.uuid.invalid"));
  private static final Collection<String> EXAMPLES =
      List.of("dd12be42-52a9-4a91-a8a1-11c01849e498");
  private static final Pattern ALLOWED_CHARACTERS_UUID =
      Pattern.compile(
          "^([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})$");
  private static final Pattern ALLOWED_CHARACTERS_ID = Pattern.compile("^(\\w+)");
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private Map<String, DialogDataEntry> dialogDataEntriesCache = new HashMap<>();

  public DialogArgument() {
    super();
  }

  public static DialogArgument uuidOrLabel() {
    return new DialogArgument();
  }

  public static Pair<UUID, String> getUuidOrLabel(
      CommandContext<CommandSourceStack> commandContext, String dialog) {
    return commandContext.getArgument(dialog, Pair.class);
  }

  @Override
  public Pair<UUID, String> parse(StringReader stringReader) throws CommandSyntaxException {
    String input = stringReader.getRemaining();
    Matcher uuidMatcher = ALLOWED_CHARACTERS_UUID.matcher(input);
    Matcher idMatcher = ALLOWED_CHARACTERS_ID.matcher(input);

    // Check if we have a valid UUID.
    if (uuidMatcher.find()) {
      String uuidString = uuidMatcher.group(1);
      try {
        UUID uuid = UUID.fromString(uuidString);
        stringReader.setCursor(stringReader.getCursor() + uuidString.length());
        return new Pair<>(uuid, "");
      } catch (IllegalArgumentException exception) {
        log.error("Found invalid dialog uuid {}", uuidString);
      }
    }

    // Check if we have a valid label and try to find the corresponding UUID.
    if (idMatcher.find()) {
      String idString = idMatcher.group(1);
      if (dialogDataEntriesCache != null
          && !dialogDataEntriesCache.isEmpty()
          && dialogDataEntriesCache.containsKey(idString)) {
        DialogDataEntry dialogDataEntry = dialogDataEntriesCache.get(idString);
        log.info("Found dialog id {}: {}", idString, dialogDataEntry);
        UUID uuid = dialogDataEntry.getId();
        stringReader.setCursor(stringReader.getCursor() + uuid.toString().length());
        return new Pair<>(uuid, idString);
      }

      // Return the id string if no UUID was found.
      stringReader.setCursor(stringReader.getCursor() + idString.length());
      return new Pair<>(null, idString);
    }

    throw ERROR_INVALID_UUID.create();
  }

  @Override
  public <S> CompletableFuture<Suggestions> listSuggestions(
      CommandContext<S> context, SuggestionsBuilder suggestionsBuilder) {
    // Get the UUID of the entity selector.
    EntitySelector entitySelector = context.getArgument("target", EntitySelector.class);
    UUID entityUUID = null;
    try {
      Field entityUUIDField = entitySelector.getClass().getDeclaredField("entityUUID");
      entityUUIDField.setAccessible(true);
      entityUUID = (UUID) entityUUIDField.get(entitySelector);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      log.error("Failed to get entity UUID from entity selector {}: {}", entitySelector, e);
    }

    // Limit suggestions to the entity UUID, if available.
    if (entityUUID != null && DialogDataManager.hasDialogDataSet(entityUUID)) {
      DialogDataSet dialogDataSet = DialogDataManager.getDialogDataSet(entityUUID);
      Map<String, DialogDataEntry> dialogDataEntries = dialogDataSet.getDialogByLabelMap();
      dialogDataEntriesCache = dialogDataEntries;
      return SharedSuggestionProvider.suggest(dialogDataEntries.keySet(), suggestionsBuilder);
    }
    return SharedSuggestionProvider.suggest(new HashSet<>(), suggestionsBuilder);
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
