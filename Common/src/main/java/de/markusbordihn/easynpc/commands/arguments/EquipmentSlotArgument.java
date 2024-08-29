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

import com.google.common.collect.Maps;
import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import net.minecraft.Util;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EquipmentSlot;

public class EquipmentSlotArgument implements ArgumentType<EquipmentSlot> {
  private static final Collection<String> EXAMPLES =
      Arrays.asList("weapon.mainhand", "weapon.offhand");
  private static final DynamicCommandExceptionType ERROR_UNKNOWN_SLOT =
      new DynamicCommandExceptionType(slot -> Component.translatable("slot.unknown", slot));
  private static final Map<String, EquipmentSlot> SLOTS =
      Util.make(
          Maps.newHashMap(),
          (map) -> {
            map.put("weapon", EquipmentSlot.MAINHAND);
            map.put("weapon.mainhand", EquipmentSlot.MAINHAND);
            map.put("weapon.offhand", EquipmentSlot.OFFHAND);
            map.put("armor.head", EquipmentSlot.HEAD);
            map.put("armor.chest", EquipmentSlot.CHEST);
            map.put("armor.legs", EquipmentSlot.LEGS);
            map.put("armor.feet", EquipmentSlot.FEET);
          });

  public EquipmentSlotArgument() {}

  public static EquipmentSlotArgument slot() {
    return new EquipmentSlotArgument();
  }

  public static EquipmentSlot getEquipmentSlot(
      final CommandContext<CommandSourceStack> commandContext, final String slotName) {
    return commandContext.getArgument(slotName, EquipmentSlot.class);
  }

  @Override
  public EquipmentSlot parse(final StringReader stringReader) throws CommandSyntaxException {
    String key = stringReader.readUnquotedString();
    if (!SLOTS.containsKey(key)) {
      throw ERROR_UNKNOWN_SLOT.create(key);
    } else {
      return SLOTS.get(key);
    }
  }

  @Override
  public <S> CompletableFuture<Suggestions> listSuggestions(
      final CommandContext<S> commandContext, final SuggestionsBuilder suggestionsBuilder) {
    return SharedSuggestionProvider.suggest(SLOTS.keySet(), suggestionsBuilder);
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
