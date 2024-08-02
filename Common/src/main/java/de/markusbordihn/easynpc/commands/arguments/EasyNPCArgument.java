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
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.commands.selector.EasyNPCSelectorParser;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.selector.EntitySelector;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;

public class EasyNPCArgument implements ArgumentType<EntitySelector> {

  public static final SimpleCommandExceptionType NO_ENTITIES_FOUND =
      new SimpleCommandExceptionType(Component.translatable("argument.entity.notfound.entity"));
  private static final Collection<String> EXAMPLES =
      Arrays.asList(
          "EasyNPC", "0123", "@e", "@e[type=foo]", "dd12be42-52a9-4a91-a8a1-11c01849e498");

  public EasyNPCArgument() {}

  public static EasyNPCArgument npc() {
    return new EasyNPCArgument();
  }

  public static EasyNPC<?> getEntityWithAccess(
      CommandContext<CommandSourceStack> context, String name) throws CommandSyntaxException {

    // Get the entity and check if it has access.
    EasyNPC<?> easyNPC = getEntitiesWithAccess(context, name).stream().findFirst().orElse(null);
    if (easyNPC == null) {
      throw NO_ENTITIES_FOUND.create();
    }

    return easyNPC;
  }

  public static Collection<? extends EasyNPC<?>> getEntitiesWithAccess(
      CommandContext<CommandSourceStack> context, String name) throws CommandSyntaxException {

    // Get all entities and filter out the ones without access.
    Collection<? extends EasyNPC<?>> easyNPCs = getEntities(context, name);
    if (!easyNPCs.isEmpty()) {
      for (EasyNPC<?> easyNPC : easyNPCs) {
        if (!AccessManager.hasAccess(context.getSource(), easyNPC.getUUID())) {
          easyNPCs = easyNPCs.stream().filter(e -> e != easyNPC).toList();
        }
      }
    }

    if (easyNPCs.isEmpty()) {
      throw NO_ENTITIES_FOUND.create();
    }

    return easyNPCs;
  }

  public static EasyNPC<?> getEntity(CommandContext<CommandSourceStack> context, String name)
      throws CommandSyntaxException {
    EasyNPC<?> easyNPC = getEntities(context, name).stream().findFirst().orElse(null);

    if (easyNPC == null) {
      throw NO_ENTITIES_FOUND.create();
    }
    return easyNPC;
  }

  public static Collection<? extends EasyNPC<?>> getEntities(
      CommandContext<CommandSourceStack> context, String name) throws CommandSyntaxException {
    Collection<? extends EasyNPC<?>> entities = getOptionalEntities(context, name);
    if (entities.isEmpty()) {
      throw NO_ENTITIES_FOUND.create();
    }

    return entities;
  }

  public static Collection<? extends EasyNPC<?>> getOptionalEntities(
      CommandContext<CommandSourceStack> context, String name) throws CommandSyntaxException {
    Collection<? extends Entity> entities =
        context.getArgument(name, EntitySelector.class).findEntities(context.getSource());
    Collection<EasyNPC<?>> easyNPCs = new ArrayList<>();
    for (Entity entity : entities) {
      if (entity instanceof EasyNPC<?> easyNPC) {
        easyNPCs.add(easyNPC);
      }
    }
    return easyNPCs;
  }

  @Override
  public EntitySelector parse(StringReader stringReader) throws CommandSyntaxException {
    EasyNPCSelectorParser entitySelectorParser = new EasyNPCSelectorParser(stringReader);
    return entitySelectorParser.parse();
  }

  @Override
  public <S> CompletableFuture<Suggestions> listSuggestions(
      CommandContext<S> context, SuggestionsBuilder suggestionsBuilder) {
    if (!(context.getSource() instanceof SharedSuggestionProvider sharedSuggestionProvider)) {
      return Suggestions.empty();
    }
    StringReader stringReader = new StringReader(suggestionsBuilder.getInput());
    stringReader.setCursor(suggestionsBuilder.getStart());
    EasyNPCSelectorParser easyNPCSelectorParser =
        new EasyNPCSelectorParser(stringReader, sharedSuggestionProvider.hasPermission(2));
    try {
      easyNPCSelectorParser.parse();
    } catch (CommandSyntaxException exception) {
      // Ignore
    }

    return easyNPCSelectorParser.fillSuggestions(
        suggestionsBuilder,
        consumer -> {
          Iterable<String> result = sharedSuggestionProvider.getSelectedEntities();
          SharedSuggestionProvider.suggest(result, consumer);
        });
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
