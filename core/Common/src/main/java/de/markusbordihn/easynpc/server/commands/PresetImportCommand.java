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

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.suggestion.PresetSuggestions;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.security.ActorSecurityContext;
import de.markusbordihn.easynpc.security.CommandSecurity;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

class PresetImportCommand extends Command {

  private static final String CUSTOM_ARG = "custom";
  private static final String DATA_ARG = "data";
  private static final String DEFAULT_ARG = "default";
  private static final String IMPORT_ARG = "import";
  private static final String IMPORT_NEW_ARG = "import_new";
  private static final String IMPORT_WITH_OWNER_ARG = "import_with_owner";
  private static final String LOCAL_ARG = "local";
  private static final String LOCATION_ARG = "location";
  private static final String PRESET_ARG = "preset";
  private static final String UUID_ARG = "uuid";
  private static final String WORLD_ARG = "world";

  private PresetImportCommand() {}

  static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal(IMPORT_ARG)
        .then(
            Commands.literal(LOCAL_ARG).executes(context -> importLocalPreset(context.getSource())))
        .then(
            importBranch(
                CUSTOM_ARG,
                PresetSuggestions::suggestCustom,
                PresetImportCommand::importCustomPreset,
                PresetImportCommand::importCustomPreset))
        .then(
            importBranch(
                DATA_ARG,
                PresetSuggestions::suggestData,
                PresetImportCommand::importDefaultPreset,
                PresetImportCommand::importDataPreset))
        .then(
            importBranch(
                DEFAULT_ARG,
                PresetSuggestions::suggestDefault,
                PresetImportCommand::importDefaultPreset,
                PresetImportCommand::importDefaultPreset))
        .then(
            importBranch(
                WORLD_ARG,
                PresetSuggestions::suggestWorld,
                PresetImportCommand::importWorldPreset,
                PresetImportCommand::importWorldPreset));
  }

  static ArgumentBuilder<CommandSourceStack, ?> registerNew() {
    return Commands.literal(IMPORT_NEW_ARG)
        .then(
            importNewBranch(
                CUSTOM_ARG,
                PresetSuggestions::suggestCustom,
                PresetImportCommand::importCustomPreset,
                PresetImportCommand::importCustomPreset))
        .then(
            importNewBranch(
                DATA_ARG,
                PresetSuggestions::suggestData,
                PresetImportCommand::importDefaultPreset,
                PresetImportCommand::importDataPreset))
        .then(
            importNewBranch(
                DEFAULT_ARG,
                PresetSuggestions::suggestDefault,
                PresetImportCommand::importDefaultPreset,
                PresetImportCommand::importDefaultPreset))
        .then(
            importNewBranch(
                WORLD_ARG,
                PresetSuggestions::suggestWorld,
                PresetImportCommand::importWorldPreset,
                PresetImportCommand::importWorldPreset));
  }

  static ArgumentBuilder<CommandSourceStack, ?> registerWithOwner() {
    return Commands.literal(IMPORT_WITH_OWNER_ARG)
        .then(
            importWithOwnerBranch(
                CUSTOM_ARG,
                PresetSuggestions::suggestCustom,
                PresetImportCommand::importCustomPreset))
        .then(
            importWithOwnerBranch(
                DATA_ARG, PresetSuggestions::suggestData, PresetImportCommand::importDataPreset))
        .then(
            importWithOwnerBranch(
                DEFAULT_ARG,
                PresetSuggestions::suggestDefault,
                PresetImportCommand::importDefaultPreset))
        .then(
            importWithOwnerBranch(
                WORLD_ARG,
                PresetSuggestions::suggestWorld,
                PresetImportCommand::importWorldPreset));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> importBranch(
      String presetType,
      SuggestionProvider<CommandSourceStack> suggestions,
      PresetImporter importWithoutPosition,
      PresetImporter importWithPosition) {
    return Commands.literal(presetType)
        .then(
            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                .suggests(suggestions)
                .executes(
                    context ->
                        importWithoutPosition.importPreset(
                            context.getSource(),
                            ResourceLocationArgument.getId(context, PRESET_ARG),
                            null,
                            null,
                            null))
                .then(
                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                        .executes(
                            context ->
                                importWithPosition.importPreset(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(context, PRESET_ARG),
                                    getPosition(context),
                                    null,
                                    null))
                        .then(
                            Commands.argument(UUID_ARG, UuidArgument.uuid())
                                .executes(
                                    context ->
                                        importWithPosition.importPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            getPosition(context),
                                            UuidArgument.getUuid(context, UUID_ARG),
                                            null)))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> importNewBranch(
      String presetType,
      SuggestionProvider<CommandSourceStack> suggestions,
      PresetImporter importWithoutPosition,
      PresetImporter importWithPosition) {
    return Commands.literal(presetType)
        .then(
            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                .suggests(suggestions)
                .executes(
                    context ->
                        importWithoutPosition.importPreset(
                            context.getSource(),
                            ResourceLocationArgument.getId(context, PRESET_ARG),
                            null,
                            UUID.randomUUID(),
                            null))
                .then(
                    Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                        .executes(
                            context ->
                                importWithPosition.importPreset(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(context, PRESET_ARG),
                                    getPosition(context),
                                    UUID.randomUUID(),
                                    null))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> importWithOwnerBranch(
      String presetType,
      SuggestionProvider<CommandSourceStack> suggestions,
      PresetImporter presetImporter) {
    return Commands.literal(presetType)
        .then(
            Commands.argument(PRESET_ARG, ResourceLocationArgument.id())
                .suggests(suggestions)
                .then(
                    Commands.argument(PLAYER_ARG, EntityArgument.player())
                        .then(
                            Commands.argument(LOCATION_ARG, Vec3Argument.vec3())
                                .executes(
                                    context ->
                                        presetImporter.importPreset(
                                            context.getSource(),
                                            ResourceLocationArgument.getId(context, PRESET_ARG),
                                            getPosition(context),
                                            UUID.randomUUID(),
                                            EntityArgument.getPlayer(context, PLAYER_ARG))))));
  }

  private static Vec3 getPosition(CommandContext<CommandSourceStack> context) {
    Coordinates coordinates = Vec3Argument.getCoordinates(context, LOCATION_ARG);
    return coordinates.getPosition(context.getSource());
  }

  private static int importCustomPreset(
      CommandSourceStack context,
      ResourceLocation preset,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!importPreset(context, PresetType.CUSTOM, preset, position, uuid, serverPlayer)) {
      return sendFailureMessage(context, importedPresetFailedMessage(CUSTOM_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(CUSTOM_ARG, preset, position, uuid));
  }

  private static int importDataPreset(
      CommandSourceStack context,
      ResourceLocation preset,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!importPreset(context, PresetType.DATA, preset, position, uuid, serverPlayer)) {
      return sendFailureMessage(context, importedPresetFailedMessage(DATA_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(DATA_ARG, preset, position, uuid));
  }

  private static int importDefaultPreset(
      CommandSourceStack context,
      ResourceLocation preset,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!importPreset(context, PresetType.DEFAULT, preset, position, uuid, serverPlayer)) {
      return sendFailureMessage(context, importedPresetFailedMessage(DEFAULT_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(DEFAULT_ARG, preset, position, uuid));
  }

  private static int importLocalPreset(CommandSourceStack context) {
    return sendFailureMessage(
        context, "Importing a local preset from the server is not supported!");
  }

  private static int importWorldPreset(
      CommandSourceStack context,
      ResourceLocation preset,
      Vec3 position,
      UUID uuid,
      ServerPlayer serverPlayer) {
    if (preset == null) {
      return Command.FAILURE;
    }

    if (!importPreset(context, PresetType.WORLD, preset, position, uuid, serverPlayer)) {
      return sendFailureMessage(context, importedPresetFailedMessage(WORLD_ARG, preset));
    }

    return sendSuccessMessage(context, importedPresetMessage(WORLD_ARG, preset, position, uuid));
  }

  private static boolean importPreset(
      CommandSourceStack context,
      PresetType presetType,
      ResourceLocation preset,
      Vec3 position,
      UUID uuid,
      ServerPlayer requestedOwner) {
    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(context);
    ServerPlayer owner = requestedOwner;
    if (owner == null && actorSecurityContext != null) {
      owner = actorSecurityContext.player();
    }

    return PresetHandler.importPreset(
        context.getLevel(), presetType, preset, position, uuid, actorSecurityContext, owner);
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

  @FunctionalInterface
  private interface PresetImporter {
    int importPreset(
        CommandSourceStack context,
        ResourceLocation preset,
        Vec3 position,
        UUID uuid,
        ServerPlayer serverPlayer);
  }
}
