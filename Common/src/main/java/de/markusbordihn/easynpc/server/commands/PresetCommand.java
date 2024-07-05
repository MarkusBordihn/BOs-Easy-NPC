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
import de.markusbordihn.easynpc.commands.suggestion.PresetSuggestions;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class PresetCommand extends Command {

  private static final String COMMAND_NAME = "preset";
  private static final String CUSTOM_ARG = "custom";
  private static final String DATA_ARG = "data";
  private static final String DEFAULT_ARG = "default";
  private static final String EXPORT_ARG = "export";
  private static final String IMPORT_ARG = "import";
  private static final String IMPORT_NEW_ARG = "import_new";
  private static final String LOCAL_ARG = "local";
  private static final String LOCATION_ARG = "location";
  private static final String PRESET_ARG = "preset";
  private static final String TARGET_ARG = "target";
  private static final String NAME_ARG = "name";
  private static final String UUID_ARG = "uuid";
  private static final String WORLD_ARG = "world";

  private PresetCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal(COMMAND_NAME)
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal(EXPORT_ARG)
                .then(
                    Commands.literal(LOCAL_ARG)
                        .then(
                            Commands.argument(TARGET_ARG, new EasyNPCArgument())
                                .executes(
                                    context ->
                                        exportLocalPreset(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, TARGET_ARG),
                                            null))
                                .then(
                                    Commands.argument(NAME_ARG, StringArgumentType.string())
                                        .executes(
                                            context ->
                                                exportLocalPreset(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, NAME_ARG))))))
                .then(
                    Commands.literal(CUSTOM_ARG)
                        .then(
                            Commands.argument(TARGET_ARG, new EasyNPCArgument())
                                .executes(
                                    context ->
                                        exportCustomPreset(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, TARGET_ARG),
                                            null))
                                .then(
                                    Commands.argument(NAME_ARG, StringArgumentType.string())
                                        .executes(
                                            context ->
                                                exportCustomPreset(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, NAME_ARG))))))
                .then(
                    Commands.literal(WORLD_ARG)
                        .then(
                            Commands.argument(TARGET_ARG, new EasyNPCArgument())
                                .executes(
                                    context ->
                                        exportWorldPreset(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, TARGET_ARG),
                                            null))
                                .then(
                                    Commands.argument(NAME_ARG, StringArgumentType.string())
                                        .executes(
                                            context ->
                                                exportWorldPreset(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, NAME_ARG)))))))
        .then(
            Commands.literal(IMPORT_ARG)
                .then(
                    Commands.literal(CUSTOM_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestCustom)
                                .executes(
                                    context ->
                                        importCustomPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            null))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importCustomPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  null);
                                            })
                                        .then(
                                            Commands.argument(UUID_ARG, UuidArgument.uuid())
                                                .executes(
                                                    context -> {
                                                      UUID uuid =
                                                          UuidArgument.getUuid(context, UUID_ARG);
                                                      Coordinates coordinates =
                                                          Vec3Argument.getCoordinates(
                                                              context, LOCATION_ARG);
                                                      return importCustomPreset(
                                                          context.getSource(),
                                                          ResourceLocationArgument.getId(
                                                              context, PRESET_ARG),
                                                          coordinates.getPosition(
                                                              context.getSource()),
                                                          uuid);
                                                    })))))
                .then(
                    Commands.literal(DATA_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestData)
                                .executes(
                                    context ->
                                        importDefaultPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            null))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importDataPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  null);
                                            })
                                        .then(
                                            Commands.argument(UUID_ARG, UuidArgument.uuid())
                                                .executes(
                                                    context -> {
                                                      UUID uuid =
                                                          UuidArgument.getUuid(context, UUID_ARG);
                                                      Coordinates coordinates =
                                                          Vec3Argument.getCoordinates(
                                                              context, LOCATION_ARG);
                                                      return importDataPreset(
                                                          context.getSource(),
                                                          ResourceLocationArgument.getId(
                                                              context, PRESET_ARG),
                                                          coordinates.getPosition(
                                                              context.getSource()),
                                                          uuid);
                                                    })))))
                .then(
                    Commands.literal(DEFAULT_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestDefault)
                                .executes(
                                    context ->
                                        importDefaultPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            null))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importDefaultPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  null);
                                            })
                                        .then(
                                            Commands.argument(UUID_ARG, UuidArgument.uuid())
                                                .executes(
                                                    context -> {
                                                      UUID uuid =
                                                          UuidArgument.getUuid(context, UUID_ARG);
                                                      Coordinates coordinates =
                                                          Vec3Argument.getCoordinates(
                                                              context, LOCATION_ARG);
                                                      return importDefaultPreset(
                                                          context.getSource(),
                                                          ResourceLocationArgument.getId(
                                                              context, PRESET_ARG),
                                                          coordinates.getPosition(
                                                              context.getSource()),
                                                          uuid);
                                                    })))))
                .then(
                    Commands.literal(WORLD_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestWorld)
                                .executes(
                                    context ->
                                        importWorldPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            null))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importWorldPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  null);
                                            })
                                        .then(
                                            Commands.argument(UUID_ARG, UuidArgument.uuid())
                                                .executes(
                                                    context -> {
                                                      UUID uuid =
                                                          UuidArgument.getUuid(context, UUID_ARG);
                                                      Coordinates coordinates =
                                                          Vec3Argument.getCoordinates(
                                                              context, LOCATION_ARG);
                                                      return importWorldPreset(
                                                          context.getSource(),
                                                          ResourceLocationArgument.getId(
                                                              context, PRESET_ARG),
                                                          coordinates.getPosition(
                                                              context.getSource()),
                                                          uuid);
                                                    }))))))
        .then(
            Commands.literal(IMPORT_NEW_ARG)
                .then(
                    Commands.literal(CUSTOM_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestCustom)
                                .executes(
                                    context ->
                                        importCustomPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            UUID.randomUUID()))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importCustomPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  UUID.randomUUID());
                                            }))))
                .then(
                    Commands.literal(DATA_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestData)
                                .executes(
                                    context ->
                                        importDefaultPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            UUID.randomUUID()))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importDataPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  UUID.randomUUID());
                                            }))))
                .then(
                    Commands.literal(DEFAULT_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestDefault)
                                .executes(
                                    context ->
                                        importDefaultPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            UUID.randomUUID()))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importDefaultPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  UUID.randomUUID());
                                            }))))
                .then(
                    Commands.literal(WORLD_ARG)
                        .then(
                            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                                .suggests(PresetSuggestions::suggestWorld)
                                .executes(
                                    context ->
                                        importWorldPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            null,
                                            UUID.randomUUID()))
                                .then(
                                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, LOCATION_ARG);
                                              return importWorldPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, PRESET_ARG),
                                                  coordinates.getPosition(context.getSource()),
                                                  UUID.randomUUID());
                                            })))));
  }

  private static int importCustomPreset(
      CommandSourceStack context, ResourceLocation preset, Vec3 position, UUID uuid) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.importCustomPreset(context.getLevel(), preset, position, uuid)) {
      return sendFailureMessage(context, importedPresetFailedMessage(CUSTOM_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(CUSTOM_ARG, preset, position, uuid));
  }

  private static int importDataPreset(
      CommandSourceStack context, ResourceLocation preset, Vec3 position, UUID uuid) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.importDataPreset(context.getLevel(), preset, position, uuid)) {
      return sendFailureMessage(context, importedPresetFailedMessage(DATA_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(DATA_ARG, preset, position, uuid));
  }

  private static int importDefaultPreset(
      CommandSourceStack context, ResourceLocation preset, Vec3 position, UUID uuid) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.importDefaultPreset(context.getLevel(), preset, position, uuid)) {
      return sendFailureMessage(context, importedPresetFailedMessage(DEFAULT_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(DEFAULT_ARG, preset, position, uuid));
  }

  private static int importWorldPreset(
      CommandSourceStack context, ResourceLocation preset, Vec3 position, UUID uuid) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!PresetHandler.importWorldPreset(context.getLevel(), preset, position, uuid)) {
      return sendFailureMessage(context, importedPresetFailedMessage(WORLD_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(WORLD_ARG, preset, position, uuid));
  }

  private static String importedPresetFailedMessage(String presetType, ResourceLocation preset) {
    return "Unable to import " + presetType + " preset " + preset + " !";
  }

  private static String importedPresetMessage(
      String presetType, ResourceLocation preset, Vec3 position, UUID uuid) {
    return "Imported "
        + presetType
        + " preset "
        + preset
        + (position != null ? " at " + position : "")
        + (uuid != null ? " with UUID " + uuid : "")
        + " !";
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
            + easyNPC.getUUID()
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
            + easyNPC.getUUID()
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
        CustomPresetDataFiles.getPresetFileName(
            name != null && !name.isEmpty() ? name : easyNPC.getUUID().toString());
    NetworkMessageHandlerManager.getClientHandler()
        .exportClientPreset(easyNPC.getUUID(), presetFileName, serverPlayer);
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
}
