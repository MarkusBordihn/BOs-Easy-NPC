/**
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

import java.io.IOException;
import java.nio.file.Path;
import java.util.UUID;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

import de.markusbordihn.easynpc.data.WorldPresetData;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;

public class PresetCommand extends CustomCommand {

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("preset")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(0))
        .executes(PresetCommand::overview)
        .then(Commands.literal("export").then(Commands.argument("uuid", UuidArgument.uuid())
            .suggests(PresetCommand::suggestEasyNPCs).executes(context -> {
              return exportPreset(context.getSource(), UuidArgument.getUuid(context, "uuid"));
            })))
        .then(Commands.literal("import")
            .then(Commands.argument("presetLocation", ResourceLocationArgument.id())
                .suggests(PresetCommand::suggestPresets).executes(context -> {
                  return importPreset(context.getSource(),
                      ResourceLocationArgument.getId(context, "presetLocation"));
                })));
  }

  private static int overview(CommandContext<CommandSourceStack> context) {
    context.getSource().sendSuccess(Component.literal("Preset command is not implemented yet!"),
        false);
    return 0;
  }

  private static int exportPreset(CommandSourceStack context, UUID uuid)
      throws CommandSyntaxException {
    ServerPlayer serverPlayer = context.getPlayerOrException();
    if (uuid == null) {
      return 0;
    }

    // Try to get the EasyNPC entity by UUID.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid);
    if (easyNPCEntity == null) {
      context.sendFailure(Component.literal("EasyNPC with UUID " + uuid + " not found!"));
      return 0;
    }

    // Check is player is owner of the EasyNPC.
    if (!serverPlayer.isCreative() && !easyNPCEntity.isOwner(serverPlayer)) {
      context.sendFailure(Component.literal("You are not the owner of this EasyNPC!"));
      return 0;
    }

    log.info("Exporting EasyNPC {} with UUID {} and skin {}...", easyNPCEntity.getName(), uuid,
        easyNPCEntity.getSkinModel());
    NetworkMessage.exportPresetClient(uuid, serverPlayer);
    return Command.SINGLE_SUCCESS;
  }

  private static int importPreset(CommandSourceStack context, ResourceLocation preset) {
    if (preset == null) {
      return 0;
    }

    // Check if preset exists.
    Path presetPath = WorldPresetData.getPresetsResourceLocationPath(preset);
    log.info("Importing preset {} {}...", preset, presetPath);
    if (!presetPath.toFile().exists()) {
      context.sendFailure(Component.literal("Preset file " + preset + " not found!"));
      return 0;
    }

    // Read preset file and create compound tag.
    CompoundTag compoundTag;
    try {
      compoundTag = NbtIo.readCompressed(presetPath.toFile());
    } catch (IOException exception) {
      context.sendFailure(Component.literal("Unable to read data from preset " + preset + "!"));
      return 0;
    }
    if (compoundTag == null) {
      context.sendFailure(Component.literal("Data from preset " + preset + " are empty!"));
      return 0;
    }

    // Get entity type.
    EntityType<?> entityType =
        compoundTag.contains("id") ? EntityType.byString(compoundTag.getString("id")).orElse(null)
            : null;
    if (entityType == null) {
      context
          .sendFailure(Component.literal("Unable to get entity type from preset " + preset + "!"));
      return 0;
    }

    // Verify compound tag.
    log.info("Importing preset {} {} {} ...", preset, compoundTag, entityType);

    // Spawn entity and overwrite preset values.
    Entity entity = entityType.create(context.getLevel());
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      easyNPCEntity.importPreset(compoundTag);
      context.getLevel().addFreshEntity(entity);
      context.sendSuccess(Component.literal("Imported preset " + preset + " to " + easyNPCEntity),
          false);
      return Command.SINGLE_SUCCESS;
    } else {
      context.sendFailure(Component.literal("Preset " + preset + " is not valid!"));
      return 0;
    }

    // Check if preset is valid.

  }

}
