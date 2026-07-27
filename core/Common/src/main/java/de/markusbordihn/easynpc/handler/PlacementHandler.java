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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import java.util.Optional;
import java.util.function.Predicate;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlacementHandler {

  public static final int DEFAULT_SEARCH_RADIUS = 4;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private PlacementHandler() {}

  public static Optional<BlockPos> findSafeSpawnNear(
      LevelReader level, Vec3 position, EntityDimensions dimensions) {
    return findSafeSpawnNear(
        level, BlockPos.containing(position), dimensions, DEFAULT_SEARCH_RADIUS);
  }

  public static Optional<BlockPos> findSafeSpawnNear(
      LevelReader level, BlockPos blockPos, EntityDimensions dimensions, int searchRadius) {
    return findSafeSpawnNear(level, blockPos, dimensions, searchRadius, candidate -> true);
  }

  public static Optional<BlockPos> findSafeSpawnNear(
      LevelReader level,
      BlockPos blockPos,
      EntityDimensions dimensions,
      int searchRadius,
      Predicate<BlockPos> additionalCondition) {
    if (level == null || blockPos == null || dimensions == null) {
      return Optional.empty();
    }

    for (BlockPos.MutableBlockPos candidate :
        BlockPos.spiralAround(blockPos, searchRadius, Direction.NORTH, Direction.EAST)) {
      BlockPos spawnPosition = candidate.immutable();
      if (isFree(level, spawnPosition, dimensions) && additionalCondition.test(spawnPosition)) {
        return Optional.of(spawnPosition);
      }
    }

    return Optional.empty();
  }

  public static boolean isFree(LevelReader level, BlockPos blockPos, EntityDimensions dimensions) {
    return level.noCollision(boundingBoxAt(blockPos, dimensions));
  }

  public static boolean isUnoccupied(Level level, BlockPos blockPos, EntityDimensions dimensions) {
    return level.getEntitiesOfClass(Entity.class, boundingBoxAt(blockPos, dimensions)).isEmpty();
  }

  public static AABB boundingBoxAt(BlockPos blockPos, EntityDimensions dimensions) {
    return dimensions.makeBoundingBox(
        blockPos.getX() + 0.5D, (double) blockPos.getY(), blockPos.getZ() + 0.5D);
  }
}
