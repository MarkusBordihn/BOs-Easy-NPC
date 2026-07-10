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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.DataFileHandler;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;

class PresetExportCommand extends Command {

  private static final String CUSTOM_ARG = "custom";
  private static final String EXPORT_ARG = "export";
  private static final String LOCAL_ARG = "local";
  private static final String WORLD_ARG = "world";

  private PresetExportCommand() {}

  static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal(EXPORT_ARG)
        .then(exportBranch(LOCAL_ARG, PresetExportCommand::exportLocalPreset))
        .then(exportBranch(CUSTOM_ARG, PresetExportCommand::exportCustomPreset))
        .then(exportBranch(WORLD_ARG, PresetExportCommand::exportWorldPreset));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> exportBranch(
      String presetType, PresetExporter presetExporter) {
    return Commands.literal(presetType)
        .then(
            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                .executes(
                    context ->
                        presetExporter.export(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            null))
                .then(
                    Commands.argument(NAME_ARG, StringArgumentType.string())
                        .executes(
                            context ->
                                presetExporter.export(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    StringArgumentType.getString(context, NAME_ARG)))));
  }

  private static int exportCustomPreset(
      CommandSourceStack context, EasyNPC<?> easyNPC, String name) {
    if (easyNPC == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.exportCustomPreset(easyNPC, name)) {
      return sendFailureMessage(context, "Unable to export custom preset for " + easyNPC + "!");
    }

    return sendSuccessMessage(
        context,
        "Exporting custom preset "
            + name
            + " for "
            + easyNPC
            + " with UUID "
            + easyNPC.getEntityUUID()
            + "!");
  }

  private static int exportWorldPreset(
      CommandSourceStack context, EasyNPC<?> easyNPC, String name) {
    if (easyNPC == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.exportWorldPreset(easyNPC, name)) {
      return sendFailureMessage(context, "Unable to export world preset for " + easyNPC + "!");
    }

    return sendSuccessMessage(
        context,
        "Exporting world preset "
            + name
            + " for "
            + easyNPC
            + " with UUID "
            + easyNPC.getEntityUUID()
            + "!");
  }

  private static int exportLocalPreset(
      CommandSourceStack context, EasyNPC<?> easyNPC, String name) {
    if (easyNPC == null) {
      return Command.FAILURE;
    }

    ServerPlayer serverPlayer;
    try {
      serverPlayer = context.getPlayerOrException();
    } catch (CommandSyntaxException e) {
      return sendFailureMessage(context, "This command can only be executed by a player!");
    }

    String presetFileName =
        DataFileHandler.getPresetFileName(
            name != null && !name.isEmpty() ? name : easyNPC.getEntityUUID().toString());
    if (presetFileName == null) {
      return sendFailureMessage(context, "Invalid preset file name!");
    }
    return sendSuccessMessage(
        context,
        "Exporting EasyNPC "
            + easyNPC.getEntity().getDisplayName().getString()
            + " locally to config/easy_npc/preset/"
            + easyNPC.getEasyNPCSkinData().getSkinModel().getName()
            + "/"
            + presetFileName
            + " !");
  }

  @FunctionalInterface
  private interface PresetExporter {
    int export(CommandSourceStack context, EasyNPC<?> easyNPC, String name);
  }
}
