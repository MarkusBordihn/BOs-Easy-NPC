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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SuggestionProvider {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected SuggestionProvider() {}

  // Return all EasyNPCs for creative mode or only the owned EasyNPCs of the player.
  protected static CompletableFuture<Suggestions> suggestEasyNPCs(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build)
      throws CommandSyntaxException {
    ServerPlayer serverPlayer = context.getSource().getPlayerOrException();
    return SharedSuggestionProvider.suggest(
        serverPlayer.isCreative()
            ? LivingEntityManager.getUUIDStrings()
            : LivingEntityManager.getUUIDStringsByOwner(serverPlayer),
        build);
  }

  // Return all presets for all easy NPCs.
  protected static CompletableFuture<Suggestions> suggestPresets(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        WorldPresetDataFiles.getPresetFilePathResourceLocations(), build);
  }

  // Return all variants for the given EasyNPC.
  protected static CompletableFuture<Suggestions> suggestVariants(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build, UUID uuid) {
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid);
    if (easyNPC == null) {
      return SharedSuggestionProvider.suggest(new String[0], build);
    }
    VariantData<?> variantData = easyNPC.getEasyNPCVariantData();
    if (variantData == null) {
      return SharedSuggestionProvider.suggest(new String[0], build);
    }
    return SharedSuggestionProvider.suggest(variantData.getVariantNames(), build);
  }
}
