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

package de.markusbordihn.easynpc.commands.suggestion;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.core.Registry;

public class EntityTypeSuggestions {

  private static final Set<String> deniedEntityTypes =
      Set.of(
          "minecraft:area_effect_cloud",
          "minecraft:armor_stand",
          "minecraft:arrow",
          "minecraft:boat",
          "minecraft:block_display",
          "minecraft:chest_minecart",
          "minecraft:chest_boat",
          "minecraft:command_block_minecart",
          "minecraft:dragon_fireball",
          "minecraft:egg",
          "minecraft:ender_dragon",
          "minecraft:end_crystal",
          "minecraft:ender_pearl",
          "minecraft:evoker_fangs",
          "minecraft:experience_bottle",
          "minecraft:experience_orb",
          "minecraft:eye_of_ender",
          "minecraft:falling_block",
          "minecraft:fireball",
          "minecraft:firework_rocket",
          "minecraft:fishing_bobber",
          "minecraft:furnace_minecart",
          "minecraft:glow_item_frame",
          "minecraft:hopper_minecart",
          "minecraft:item",
          "minecraft:item_frame",
          "minecraft:interaction",
          "minecraft:leash_knot",
          "minecraft:lightning_bolt",
          "minecraft:llama_spit",
          "minecraft:marker",
          "minecraft:minecart",
          "minecraft:mooshroom",
          "minecraft:painting",
          "minecraft:potion",
          "minecraft:shulker_bullet",
          "minecraft:small_fireball",
          "minecraft:snowball",
          "minecraft:spawner_minecart",
          "minecraft:spectral_arrow",
          "minecraft:text_display",
          "minecraft:tnt",
          "minecraft:tnt_minecart",
          "minecraft:trident",
          "minecraft:wither_skull");
  private static List<String> filteredEntityTypes;

  private EntityTypeSuggestions() {
  }

  public static CompletableFuture<Suggestions> suggest(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggest(getFilteredEntityTypes(), build);
  }

  private static Stream<String> getFilteredEntityTypes() {
    if (filteredEntityTypes == null) {
      filteredEntityTypes =
          Registry.ENTITY_TYPE.keySet().stream()
              .filter(
                  entityType -> {
                    String entityTypeName = entityType.toString();
                    return !entityTypeName.startsWith("easy_npc")
                        && !deniedEntityTypes.contains(entityType.toString());
                  })
              .map(entityType -> "\"" + entityType + "\"")
              .toList();
    }
    return filteredEntityTypes.stream();
  }
}
