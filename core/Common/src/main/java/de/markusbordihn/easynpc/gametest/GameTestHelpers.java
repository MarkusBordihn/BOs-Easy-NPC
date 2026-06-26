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

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;

public final class GameTestHelpers {

  private GameTestHelpers() {}

  @SuppressWarnings("unchecked")
  public static EasyNPC<?> mockEasyNPC(
      GameTestHelper helper, EntityType<?> entityType, Vec3 position) {
    Mob entity = helper.spawn((EntityType<? extends Mob>) entityType, position);
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      helper.fail("Spawned entity is not an EasyNPC: " + entity);
      return null;
    }
    if (LivingEntityManager.getEasyNPCEntityByUUID(easyNPC.getEntityUUID()) == null) {
      LivingEntityManager.addEasyNPC(easyNPC);
    }
    return easyNPC;
  }

  public static EasyNPC<?> spawnNPCEntityType(GameTestHelper helper, EntityType<?> entityType) {
    Entity entity = spawnEntityType(helper, entityType);
    if (entity instanceof EasyNPC<?> easyNPC) {
      return easyNPC;
    }
    helper.fail("Entity " + entityType + " is not an EasyNPC!");
    return null;
  }

  @SuppressWarnings("unchecked")
  public static <T extends Entity> T spawnEntityType(
      GameTestHelper helper, EntityType<?> entityType) {
    if (entityType == null) {
      helper.fail("EntityType is null!");
      return null;
    }
    Player player = helper.makeMockPlayer(GameType.DEFAULT_MODE);
    T entity = (T) entityType.create((ServerLevel) player.level(), EntitySpawnReason.COMMAND);
    if (entity == null) {
      helper.fail("Entity for " + entityType + " is null!");
      return null;
    }
    if (!player.level().addFreshEntity(entity)) {
      helper.fail("Failed to spawn entity " + entityType + "!");
      return null;
    }

    return entity;
  }

  public static void assertEquals(
      GameTestHelper helper, String message, Object expected, Object actual) {
    if (!expected.equals(actual)) {
      helper.fail(message);
    }
  }

  public static void assertTrue(GameTestHelper helper, String message, boolean condition) {
    if (!condition) {
      helper.fail(message);
    }
  }

  public static void assertNotNull(GameTestHelper helper, String message, Object object) {
    assertTrue(helper, message, object != null);
  }

  public static ServerPlayer mockServerPlayer(GameTestHelper helper, Vec3 position) {
    return new FakePlayer(
        helper.getLevel(), BlockPos.containing(position.x, position.y, position.z));
  }
}
