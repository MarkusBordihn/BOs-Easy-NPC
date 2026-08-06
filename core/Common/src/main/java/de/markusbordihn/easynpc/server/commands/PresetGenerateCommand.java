/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.data.preset.BasePresetGenerator;
import de.markusbordihn.easynpc.io.DataFileHandler;
import java.nio.file.Path;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;
import net.minecraft.server.level.ServerLevel;

class PresetGenerateCommand extends Command {

  private static final String GENERATE_ARG = "generate";
  private static final String BASE_PRESETS_ARG = "base_presets";
  private static final String GENERATED_FOLDER_NAME = "generated";

  private PresetGenerateCommand() {}

  static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal(GENERATE_ARG)
        .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_OWNER))
        .then(
            Commands.literal(BASE_PRESETS_ARG)
                .executes(context -> generateBasePresets(context.getSource())));
  }

  private static int generateBasePresets(CommandSourceStack context) {
    ServerLevel serverLevel = context.getLevel();
    Path targetFolder =
        DataFileHandler.getCustomDataFolder()
            .resolve(GENERATED_FOLDER_NAME)
            .resolve(DataFileHandler.RESOURCE_BASE_PRESET_PATH);

    int generatedPresets =
        BasePresetGenerator.generateBasePresets(serverLevel, targetFolder.toFile());
    if (generatedPresets == 0) {
      return sendFailureMessage(context, "Unable to generate any base preset!");
    }

    return sendSuccessMessage(
        context, "Generated " + generatedPresets + " base presets in " + targetFolder + "!");
  }
}
