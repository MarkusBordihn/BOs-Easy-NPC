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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.data.spawner.SpawnerData;
import java.util.List;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.SpawnerBlockEntity;

public class SpawnerCommand extends Command {

  private SpawnerCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("spawner")
        .then(
            Commands.literal("set")
                .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
                .then(
                    Commands.argument(TARGET_ARG, BlockPosArgument.blockPos())
                        .then(
                            Commands.argument(PARAMETER_ARG, StringArgumentType.string())
                                .suggests(
                                    (context, builder) ->
                                        SharedSuggestionProvider.suggest(
                                            List.of(
                                                "Delay",
                                                "MinSpawnDelay",
                                                "MaxSpawnDelay",
                                                "SpawnCount",
                                                "MaxNearbyEntities",
                                                "RequiredPlayerRange",
                                                "SpawnRange"),
                                            builder))
                                .then(
                                    Commands.argument(
                                            VALUE_ARG, IntegerArgumentType.integer(0, 1000))
                                        .executes(
                                            context ->
                                                setSpawnerValue(
                                                    context.getSource(),
                                                    BlockPosArgument.getLoadedBlockPos(
                                                        context, TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, PARAMETER_ARG),
                                                    IntegerArgumentType.getInteger(
                                                        context, VALUE_ARG)))))));
  }

  private static int setSpawnerValue(
      CommandSourceStack context, BlockPos blockPos, String parameter, int value) {
    BlockEntity blockEntity = context.getLevel().getBlockEntity(blockPos);

    // Check if block entity is a valid spawner and get spawner data.
    CompoundTag spawnerData;
    if (blockEntity instanceof SpawnerBlockEntity spawnerBlockEntity) {
      spawnerData = spawnerBlockEntity.getSpawner().save(new CompoundTag());
    } else if (blockEntity instanceof EasyNPCSpawnerBlockEntity spawnerBlockEntity) {
      spawnerData = spawnerBlockEntity.getSpawner().save(new CompoundTag());
    } else {
      return sendFailureMessage(context, "No valid spawner found at " + blockPos);
    }

    // Get and apply adjusted spawner data
    if (!SpawnerData.setSpawnerValue(spawnerData, parameter, (short) value)) {
      return sendFailureMessage(
          context, "Invalid parameter " + parameter + " for spawner at " + blockPos);
    }

    // Load adjusted spawner data
    if (blockEntity instanceof SpawnerBlockEntity spawnerBlockEntity) {
      spawnerBlockEntity
          .getSpawner()
          .load(context.getLevel(), spawnerBlockEntity.getBlockPos(), spawnerData);
      spawnerBlockEntity.setChanged();
    } else if (blockEntity instanceof EasyNPCSpawnerBlockEntity spawnerBlockEntity) {
      spawnerBlockEntity
          .getSpawner()
          .load(context.getLevel(), spawnerBlockEntity.getBlockPos(), spawnerData);
      spawnerBlockEntity.setChanged();
    }

    return sendSuccessMessage(
        context, "Adjusted spawner data " + spawnerData + " for spawner at " + blockPos);
  }
}
