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

import com.mojang.brigadier.Command;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.access.EasyNPCAccessManager;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.io.IOException;
import java.nio.file.Path;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PresetCommand {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public PresetCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("preset")
        .executes(PresetCommand::overview)
        .then(
            Commands.literal("export")
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .executes(
                            context ->
                                exportPreset(
                                    context.getSource(), UuidArgument.getUuid(context, "uuid")))))
        .then(
            Commands.literal("import")
                .then(
                    Commands.argument("presetLocation", ResourceLocationArgument.id())
                        .suggests(SuggestionProvider::suggestPresets)
                        .executes(
                            context ->
                                importPreset(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(context, "presetLocation"),
                                    null,
                                    null))
                        .then(
                            Commands.argument("location", Vec3Argument.vec3())
                                .executes(
                                    context -> {
                                      Coordinates coordinates =
                                          Vec3Argument.getCoordinates(context, "location");
                                      Vec3 vec3Position =
                                          coordinates.getPosition(context.getSource());
                                      return importPreset(
                                          context.getSource(),
                                          ResourceLocationArgument.getId(context, "presetLocation"),
                                          vec3Position,
                                          null);
                                    }))))
        .then(
            Commands.literal("import_new")
                .then(
                    Commands.argument("presetLocation", ResourceLocationArgument.id())
                        .suggests(SuggestionProvider::suggestPresets)
                        .executes(
                            context -> {
                              Vec3 vec3 = null;
                              try {
                                ServerPlayer serverPlayer =
                                    context.getSource().getPlayerOrException();
                                vec3 = serverPlayer.position();
                              } catch (CommandSyntaxException exception) {
                                log.debug(
                                    "Unable to get server player for preset import new, maybe not triggered by player!");
                              }
                              return importPreset(
                                  context.getSource(),
                                  ResourceLocationArgument.getId(context, "presetLocation"),
                                  vec3,
                                  UUID.randomUUID());
                            })
                        .then(
                            Commands.argument("location", Vec3Argument.vec3())
                                .executes(
                                    context -> {
                                      Coordinates coordinates =
                                          Vec3Argument.getCoordinates(context, "location");
                                      Vec3 vec3Position =
                                          coordinates.getPosition(context.getSource());
                                      return importPreset(
                                          context.getSource(),
                                          ResourceLocationArgument.getId(context, "presetLocation"),
                                          vec3Position,
                                          UUID.randomUUID());
                                    })
                                .then(
                                    Commands.argument("uuid", UuidArgument.uuid())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(context, "location");
                                              Vec3 vec3Position =
                                                  coordinates.getPosition(context.getSource());
                                              return importPreset(
                                                  context.getSource(),
                                                  ResourceLocationArgument.getId(
                                                      context, "presetLocation"),
                                                  vec3Position,
                                                  UuidArgument.getUuid(context, "uuid"));
                                            })))));
  }

  private static int overview(CommandContext<CommandSourceStack> context) {
    context
        .getSource()
        .sendSuccess(() -> Component.literal("Preset command is not implemented yet!"), false);
    return Command.SINGLE_SUCCESS;
  }

  private static int importPreset(
      CommandSourceStack context,
      ResourceLocation preset,
      @Nullable Vec3 position,
      @Nullable UUID uuid) {
    if (preset == null) {
      return 0;
    }
    log.info(
        "Try importing preset {} with position {} and UUID {} by {}",
        preset,
        position,
        uuid,
        context.getEntity());

    // Check if preset exists in world resources.
    CompoundTag compoundTag = null;
    Path presetPath = WorldPresetDataFiles.getPresetsResourceLocationPath(preset);
    if (presetPath != null && presetPath.toFile().exists()) {
      log.info("Importing world preset {} from {}...", preset, presetPath);
      try {
        compoundTag = NbtIo.readCompressed(presetPath.toFile());
      } catch (IOException exception) {
        log.error("Unable to read world preset {} from {}!", preset, presetPath);
        context.sendFailure(
            Component.literal(
                "Unable to read world preset " + preset + " from " + presetPath + "!"));
        return 0;
      }
    }

    // Check if preset exists in mod resources.
    if (compoundTag == null
        || compoundTag.isEmpty()
            && context.getServer().getResourceManager().getResource(preset).isPresent()) {
      try {
        log.info("Importing preset {} from mod resources...", preset);
        compoundTag = NbtIo.readCompressed(context.getServer().getResourceManager().open(preset));
      } catch (IOException exception) {
        log.error("Unable to read preset {} from mod resources!", preset);
        context.sendFailure(Component.literal("Preset file " + preset + " not found!"));
        return 0;
      }
    }

    // Read preset file and create compound tag.
    if (compoundTag.isEmpty()) {
      context.sendFailure(Component.literal("Data from preset " + preset + " are empty!"));
      return 0;
    }

    // Get entity type.
    EntityType<?> entityType =
        compoundTag.contains(Entity.ID_TAG)
            ? EntityType.byString(compoundTag.getString(Entity.ID_TAG)).orElse(null)
            : null;
    if (entityType == null) {
      context.sendFailure(
          Component.literal("Unable to get entity type from preset " + preset + "!"));
      return 0;
    }

    // Verify compound tag.
    log.info(
        "Importing preset {} with entity type {} and position {} with UUID {}",
        preset,
        entityType,
        position,
        compoundTag.getUUID(Entity.UUID_TAG));
    log.debug("Importing preset {} with compound tag {}", preset, compoundTag);

    // Overwrite spawn position if coordinates are given.
    if (position != null) {
      ListTag posTag = new ListTag();
      posTag.add(DoubleTag.valueOf(position.x));
      posTag.add(DoubleTag.valueOf(position.y));
      posTag.add(DoubleTag.valueOf(position.z));
      compoundTag.put("Pos", posTag);
    }

    // Overwrite UUID, if UUID is given.
    if (uuid != null) {
      compoundTag.putUUID(Entity.UUID_TAG, uuid);
    }

    // Get UUID from compound tag and check if entity with this UUID already exists.
    UUID existingUUID =
        compoundTag.contains(Entity.UUID_TAG) ? compoundTag.getUUID(Entity.UUID_TAG) : null;
    if (existingUUID != null
        && LivingEntityManager.getEasyNPCEntityByUUID(existingUUID, context.getLevel()) != null) {
      LivingEntityManager.discardEasyNPCEntityByUUID(existingUUID, context.getLevel());
    }

    // Spawn new entity or re-use existing entity.
    Entity entity = entityType.create(context.getLevel());
    if (entity instanceof EasyNPC<?> easyNPC) {

      // Import preset data.
      easyNPC.getEasyNPCPresetData().importPresetData(compoundTag);

      // Set home position, if spawn position was provided.
      if (position != null) {
        easyNPC
            .getEasyNPCNavigationData()
            .setHomePosition(new BlockPos((int) position.x, (int) position.y, (int) position.z));
      }

      if (context.getLevel().addFreshEntity(easyNPC.getEasyNPCEntity())) {
        context.sendSuccess(
            () -> Component.literal("Imported preset " + preset + " to " + easyNPC), false);
      } else {
        context.sendFailure(Component.literal("Unable to import preset " + preset + "!"));
      }
      return Command.SINGLE_SUCCESS;
    } else {
      context.sendFailure(Component.literal("Preset " + preset + " is not valid!"));
      return 0;
    }
  }

  private static int exportPreset(CommandSourceStack context, UUID uuid) {
    if (uuid == null) {
      return 0;
    }

    log.info("Try to exporting EasyNPC with UUID {}...", uuid);

    // Check if player is available.
    ServerPlayer serverPlayer;
    try {
      serverPlayer = context.getPlayerOrException();
    } catch (CommandSyntaxException e) {
      context.sendFailure(Component.literal("This command can only be executed by a player!"));
      return 0;
    }

    // Check if player has access to the EasyNPC.
    if (!EasyNPCAccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to export this EasyNPC!"));
      return 0;
    }

    // Get the EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid);

    log.info(
        "Exporting EasyNPC {} with UUID {} and skin {}...",
        easyNPC,
        uuid,
        easyNPC.getEasyNPCSkinData().getSkinModel());
    NetworkMessageHandlerManager.getNetworkMessageHandler().exportPresetClient(uuid, serverPlayer);
    context.sendSuccess(
        () ->
            Component.literal(
                "Exported EasyNPC "
                    + easyNPC.getEntity().getDisplayName().getString()
                    + " with UUID "
                    + uuid
                    + " locally!"),
        true);
    return Command.SINGLE_SUCCESS;
  }
}
