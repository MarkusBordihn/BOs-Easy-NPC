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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.item.ModSpawnEggItem;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public class ModSpawnEggItemTestHelper {

  private ModSpawnEggItemTestHelper() {}

  public static void useSpawnEggItem(
      GameTestHelper helper, Item item, EntityType<? extends PathfinderMob> entityType) {
    if (item instanceof ModSpawnEggItem modSpawnEggItem) {
      useSpawnEggItem(helper, modSpawnEggItem, entityType);
    } else {
      helper.fail("Item " + item + " is not a ModSpawnEggItem!");
    }
  }

  public static void useSpawnEggItem(
      GameTestHelper helper,
      ModSpawnEggItem modSpawnEggItem,
      EntityType<? extends PathfinderMob> entityType) {
    Player player = helper.makeMockPlayer();
    UseOnContext useOnContext =
        new UseOnContext(
            player,
            player.getUsedItemHand(),
            new BlockHitResult(
                helper.absoluteVec(new Vec3(1, 2, 1)),
                Direction.UP,
                helper.absolutePos(new BlockPos(1, 2, 1)),
                false));
    modSpawnEggItem.useOn(useOnContext);

    helper.assertEntityPresent(entityType, new BlockPos(1, 2, 1));
  }
}
