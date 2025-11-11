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
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.entity.ModCustomEntityType;
import de.markusbordihn.easynpc.entity.ModEntityTypeProvider;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import de.markusbordihn.easynpc.entity.ModRawEntityType;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.HorizontalDirectionalBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.SignBlockEntity;
import net.minecraft.world.level.block.entity.SignText;
import net.minecraft.world.level.block.state.BlockState;

public class TestCommand extends Command {

  private TestCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("test")
        .requires(commandSource -> commandSource.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("spawn")
                .then(
                    Commands.literal("raw_npc")
                        .executes(
                            context ->
                                spawnAllEntities(context.getSource(), ModRawEntityType.class))
                        .then(
                            Commands.argument(TYPE_ARG, StringArgumentType.word())
                                .suggests(
                                    (context, builder) -> {
                                      for (ModRawEntityType type : ModRawEntityType.values()) {
                                        builder.suggest(type.name().toLowerCase());
                                      }
                                      return builder.buildFuture();
                                    })
                                .executes(
                                    context -> {
                                      String input =
                                          StringArgumentType.getString(context, TYPE_ARG)
                                              .toUpperCase();
                                      ModRawEntityType type;
                                      try {
                                        type = ModRawEntityType.valueOf(input);
                                      } catch (IllegalArgumentException e) {
                                        throw new SimpleCommandExceptionType(
                                                Component.literal("Unknown NPC type: " + input))
                                            .create();
                                      }
                                      return spawnSingleEntity(context.getSource(), type);
                                    })))
                .then(
                    Commands.literal("standard_npc")
                        .executes(
                            context ->
                                spawnAllEntities(context.getSource(), ModNPCEntityType.class))
                        .then(
                            Commands.argument(TYPE_ARG, StringArgumentType.word())
                                .suggests(
                                    (context, builder) -> {
                                      for (ModNPCEntityType type : ModNPCEntityType.values()) {
                                        builder.suggest(type.name().toLowerCase());
                                      }
                                      return builder.buildFuture();
                                    })
                                .executes(
                                    context -> {
                                      String input =
                                          StringArgumentType.getString(context, TYPE_ARG)
                                              .toUpperCase();
                                      ModNPCEntityType type;
                                      try {
                                        type = ModNPCEntityType.valueOf(input);
                                      } catch (IllegalArgumentException e) {
                                        throw new SimpleCommandExceptionType(
                                                Component.literal("Unknown NPC type: " + input))
                                            .create();
                                      }
                                      return spawnSingleEntity(context.getSource(), type);
                                    })))
                .then(
                    Commands.literal("custom_npc")
                        .executes(
                            context ->
                                spawnAllEntities(context.getSource(), ModCustomEntityType.class))
                        .then(
                            Commands.argument(TYPE_ARG, StringArgumentType.word())
                                .suggests(
                                    (context, builder) -> {
                                      for (ModCustomEntityType type :
                                          ModCustomEntityType.values()) {
                                        builder.suggest(type.name().toLowerCase());
                                      }
                                      return builder.buildFuture();
                                    })
                                .executes(
                                    context -> {
                                      String input =
                                          StringArgumentType.getString(context, TYPE_ARG)
                                              .toUpperCase();
                                      ModCustomEntityType type;
                                      try {
                                        type = ModCustomEntityType.valueOf(input);
                                      } catch (IllegalArgumentException e) {
                                        throw new SimpleCommandExceptionType(
                                                Component.literal("Unknown NPC type: " + input))
                                            .create();
                                      }
                                      return spawnSingleEntity(context.getSource(), type);
                                    }))));
  }

  public static <T extends Enum<T> & ModEntityTypeProvider> int spawnAllEntities(
      CommandSourceStack source, Class<T> enumClass) throws CommandSyntaxException {

    ServerPlayer player = source.getPlayerOrException();
    ServerLevel level = player.serverLevel();
    BlockPos basePos = player.blockPosition().offset(0, 1, 0);

    int index = 0;
    for (T type : enumClass.getEnumConstants()) {
      int xOffset = (index % 5) * 10;
      int zOffset = (index / 5) * 10;
      BlockPos npcPos = basePos.offset(xOffset, 0, zOffset);
      EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(type.getResourceKey());
      spawnNpcInTestArea(level, npcPos, entityType);
      index++;
    }

    source.sendSuccess(
        () ->
            Component.literal(
                "Spawned "
                    + enumClass.getEnumConstants().length
                    + " "
                    + enumClass.getSimpleName()
                    + "s for testing."),
        false);
    return 1;
  }

  public static <T extends Enum<T> & ModEntityTypeProvider> int spawnSingleEntity(
      CommandSourceStack source, T type) throws CommandSyntaxException {

    ServerPlayer player = source.getPlayerOrException();
    ServerLevel level = player.serverLevel();
    BlockPos basePos = player.blockPosition().offset(0, 1, 0);
    EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(type.getResourceKey());

    spawnNpcInTestArea(level, basePos, entityType);
    source.sendSuccess(
        () -> Component.literal("Spawned " + type.getClass().getSimpleName() + ": " + type.name()),
        false);
    return 1;
  }

  private static void spawnNpcInTestArea(
      ServerLevel level, BlockPos centerPos, EntityType<?> entityType) {
    int radius = 3;

    // Clear area: from bottom to ceiling
    fillArea(
        level,
        centerPos.offset(-radius, -1, -radius),
        centerPos.offset(radius, 4, radius),
        Blocks.AIR.defaultBlockState());

    // Floor: quartz with glowstone at corners
    for (int dx = -radius; dx <= radius; dx++) {
      for (int dz = -radius; dz <= radius; dz++) {
        BlockPos floorPos = centerPos.offset(dx, -1, dz);
        if ((Math.abs(dx) == radius) && (Math.abs(dz) == radius)) {
          level.setBlockAndUpdate(floorPos, Blocks.GLOWSTONE.defaultBlockState());
        } else {
          level.setBlockAndUpdate(floorPos, Blocks.QUARTZ_BLOCK.defaultBlockState());
        }
      }
    }

    // Walls: oak fences, 1 high, except 3 high at corners
    for (int dx = -radius; dx <= radius; dx++) {
      for (int dz = -radius; dz <= radius; dz++) {
        boolean isEdge = Math.abs(dx) == radius || Math.abs(dz) == radius;
        boolean isCorner = Math.abs(dx) == radius && Math.abs(dz) == radius;
        if (isEdge) {
          for (int dy = 0; dy < (isCorner ? 3 : 1); dy++) {
            BlockPos wallPos = centerPos.offset(dx, dy, dz);
            level.setBlockAndUpdate(wallPos, Blocks.OAK_FENCE.defaultBlockState());
          }
        }
      }
    }

    // Ceiling: oak slabs at y + 3
    fillArea(
        level,
        centerPos.offset(-radius, 3, -radius),
        centerPos.offset(radius, 3, radius),
        Blocks.OAK_SLAB.defaultBlockState());

    // Additional solid area with oak planks under the sign on Y + 3
    for (int dx = -1; dx <= 1; dx++) {
      for (int dz = -1; dz <= 1; dz++) {
        BlockPos lowerSlabPos = centerPos.offset(dx, 3, dz);
        level.setBlockAndUpdate(lowerSlabPos, Blocks.OAK_PLANKS.defaultBlockState());
      }
    }

    // Add a fence gate (south side, center)
    BlockPos gatePos = centerPos.offset(0, 0, radius);
    level.setBlockAndUpdate(
        gatePos,
        Blocks.OAK_FENCE_GATE
            .defaultBlockState()
            .setValue(HorizontalDirectionalBlock.FACING, Direction.SOUTH));

    // Spawn sign with NPC id on top of the platform (centered, Y + 4)
    BlockPos signPos = centerPos.offset(0, 4, 0);
    level.setBlockAndUpdate(signPos, Blocks.OAK_SIGN.defaultBlockState());
    BlockEntity blockEntity = level.getBlockEntity(signPos);
    if (blockEntity instanceof SignBlockEntity sign) {
      sign.setText(
          new SignText()
              .setMessage(0, Component.literal("EntityType:"))
              .setMessage(1, Component.literal(entityType.toShortString())),
          false);
      sign.setChanged();
    }

    // Spawn the NPC
    entityType.spawn(level, null, null, centerPos, MobSpawnType.COMMAND, true, false);
  }

  private static void fillArea(
      ServerLevel level, BlockPos from, BlockPos to, BlockState blockState) {
    BlockPos.betweenClosedStream(from, to)
        .forEach(pos -> level.setBlockAndUpdate(pos.immutable(), blockState));
  }
}
