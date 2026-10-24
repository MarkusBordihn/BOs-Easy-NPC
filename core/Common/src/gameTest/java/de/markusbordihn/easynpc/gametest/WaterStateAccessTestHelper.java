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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.access.WaterStateAccessHelper;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class WaterStateAccessTestHelper {

  private WaterStateAccessTestHelper() {}

  public static void assertWaterStateIsWritable(GameTestHelper helper, EntityType<?> entityType) {
    Entity entity = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1)).getEntity();

    GameTestHelpers.assertTrue(
        helper,
        "Entity must expose the water state access helper",
        entity instanceof WaterStateAccessHelper);
    WaterStateAccessHelper waterStateAccess = (WaterStateAccessHelper) entity;

    waterStateAccess.setWasTouchingWater(true);
    GameTestHelpers.assertTrue(
        helper, "Entity must report water contact after it was set", entity.isInWater());

    waterStateAccess.setWasTouchingWater(false);
    GameTestHelpers.assertTrue(
        helper, "Entity must report no water contact after it was cleared", !entity.isInWater());
  }
}
