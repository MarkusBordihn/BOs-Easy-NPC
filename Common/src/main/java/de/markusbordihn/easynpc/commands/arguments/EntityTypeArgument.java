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
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public class EntityTypeArgument implements ArgumentType<EntityType<? extends Entity>> {
  private static final Collection<String> EXAMPLES =
      Arrays.asList("minecraft:pig", "minecraft:cow");
  private static final SimpleCommandExceptionType ERROR_UNKNOWN_ENTITY =
      new SimpleCommandExceptionType(Component.translatable("argument.entity.unknown"));

  public EntityTypeArgument() {}

  public static EntityTypeArgument entityType() {
    return new EntityTypeArgument();
  }

  public static EntityType<? extends Entity> getEntityType(
      CommandContext<CommandSourceStack> commandContext, String entityName) {
    return commandContext.getArgument(entityName, EntityType.class);
  }

  @Override
  public EntityType<? extends Entity> parse(StringReader stringReader)
      throws CommandSyntaxException {
    String entityName = stringReader.getRemaining();
    EntityType<? extends Entity> entityType = EntityType.byString(entityName).orElse(null);
    if (entityType != null) {
      stringReader.setCursor(stringReader.getCursor() + entityName.length());
      return entityType;
    }
    throw ERROR_UNKNOWN_ENTITY.create();
  }

  @Override
  public <S> CompletableFuture<Suggestions> listSuggestions(
      CommandContext<S> context, SuggestionsBuilder suggestionsBuilder) {
    Set<String> knownEntityTypes =
        EntityTypeManager.getSupportedEntityTypes().stream()
            .map(entityType -> Registry.ENTITY_TYPE.getKey(entityType).toString())
            .collect(Collectors.toSet());
    return SharedSuggestionProvider.suggest(knownEntityTypes, suggestionsBuilder);
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
