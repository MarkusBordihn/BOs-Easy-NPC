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
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.EquipmentSlot;

public class EquipmentSlotArgument implements ArgumentType<EquipmentSlot> {
  private static final Collection<String> EXAMPLES =
      Arrays.asList("weapon.mainhand", "weapon.offhand");
  private static final DynamicCommandExceptionType ERROR_UNKNOWN_SLOT =
      new DynamicCommandExceptionType(
          slot -> new TranslatableComponent("slot.unknown", new Object[] {slot}));
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
      CommandContext<CommandSourceStack> commandContext, String slotName) {
    return commandContext.getArgument(slotName, EquipmentSlot.class);
  }

  public EquipmentSlot parse(StringReader stringReader) throws CommandSyntaxException {
    String key = stringReader.readUnquotedString();
    if (!SLOTS.containsKey(key)) {
      throw ERROR_UNKNOWN_SLOT.create(key);
    } else {
      return SLOTS.get(key);
    }
  }

  public <S> CompletableFuture<Suggestions> listSuggestions(
      CommandContext<S> commandContext, SuggestionsBuilder suggestionsBuilder) {
    return SharedSuggestionProvider.suggest(SLOTS.keySet(), suggestionsBuilder);
  }

  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
