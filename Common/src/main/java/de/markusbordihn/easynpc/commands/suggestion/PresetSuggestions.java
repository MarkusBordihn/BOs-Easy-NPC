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
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.DataPresetDataFiles;
import de.markusbordihn.easynpc.io.DefaultPresetDataFiles;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import java.util.concurrent.CompletableFuture;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;

public class PresetSuggestions {

  private PresetSuggestions() {
  }

  public static CompletableFuture<Suggestions> suggest(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        WorldPresetDataFiles.getPresetResourceLocations(), build);
  }

  public static CompletableFuture<Suggestions> suggestCustom(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        CustomPresetDataFiles.getPresetResourceLocations(), build);
  }

  public static CompletableFuture<Suggestions> suggestData(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        DataPresetDataFiles.getPresetResourceLocations(context.getSource().getServer()), build);
  }

  public static CompletableFuture<Suggestions> suggestDefault(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        DefaultPresetDataFiles.getPresetResourceLocations(context.getSource().getServer()), build);
  }

  public static CompletableFuture<Suggestions> suggestWorld(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder build) {
    return SharedSuggestionProvider.suggestResource(
        WorldPresetDataFiles.getPresetResourceLocations(), build);
  }
}
