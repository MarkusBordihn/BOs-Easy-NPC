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

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ambient.Bat;

public class RenderEntityTypeTestHelper {

  private RenderEntityTypeTestHelper() {}

  public static void assertBatIsAvailableAsRenderEntity(GameTestHelper helper) {
    Mob renderEntity = EntityTypeManager.getRenderEntity(EntityType.BAT, helper.getLevel());

    GameTestHelpers.assertNotNull(
        helper, "Bat must be available as render entity for the Doppler", renderEntity);
    GameTestHelpers.assertTrue(
        helper, "Bat render entity must be a vanilla Bat", renderEntity instanceof Bat);
    GameTestHelpers.assertTrue(helper, "Bat render entity must have no AI", renderEntity.isNoAi());
    GameTestHelpers.assertTrue(helper, "Bat render entity must be silent", renderEntity.isSilent());
    GameTestHelpers.assertTrue(
        helper, "Bat render entity must have no physics", renderEntity.noPhysics);
    GameTestHelpers.assertTrue(
        helper,
        "Bat must be marked as supported entity type",
        EntityTypeManager.isSupportedEntityType(EntityType.BAT));
    GameTestHelpers.assertTrue(
        helper,
        "Bat must not be marked as unsupported entity type",
        !EntityTypeManager.isUnsupportedEntityType(EntityType.BAT));
  }

  public static void assertPathfinderMobLookupStaysTypeSafe(GameTestHelper helper) {
    PathfinderMob batPathfinderMob =
        EntityTypeManager.getPathfinderMob(EntityType.BAT, helper.getLevel());
    GameTestHelpers.assertTrue(
        helper, "Bat must not be returned as PathfinderMob", batPathfinderMob == null);

    GameTestHelpers.assertNotNull(
        helper,
        "Zombie must still be returned as PathfinderMob",
        EntityTypeManager.getPathfinderMob(EntityType.ZOMBIE, helper.getLevel()));
  }

  public static void assertRenderEntityIsReusedPerEntityType(GameTestHelper helper) {
    Mob firstRenderEntity = EntityTypeManager.getRenderEntity(EntityType.BAT, helper.getLevel());
    Mob secondRenderEntity = EntityTypeManager.getRenderEntity(EntityType.BAT, helper.getLevel());

    GameTestHelpers.assertTrue(
        helper,
        "Repeated lookups must reuse the same render entity",
        firstRenderEntity == secondRenderEntity);
  }
}
