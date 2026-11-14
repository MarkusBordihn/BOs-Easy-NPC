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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.lang.reflect.Method;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.AbstractSkeleton;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.phys.Vec3;

public class SunBurnTestHelper {

  private SunBurnTestHelper() {}

  public static void assertDoesNotBurnInDaylight(GameTestHelper helper, EntityType<?> npcType) {
    Zombie vanillaZombie = new Zombie(EntityType.ZOMBIE, helper.getLevel());
    GameTestHelpers.assertTrue(
        helper,
        "A vanilla zombie must be sun sensitive, otherwise this check is not meaningful",
        invokeProtectedBoolean(vanillaZombie, "isSunSensitive"));

    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, npcType, new Vec3(1, 1, 1));
    Entity npcEntity = easyNPC.getEntity();
    if (npcEntity instanceof Zombie) {
      GameTestHelpers.assertTrue(
          helper,
          npcType.getDescriptionId() + " NPC must not be sun sensitive",
          !invokeProtectedBoolean(npcEntity, "isSunSensitive"));
    } else if (npcEntity instanceof AbstractSkeleton) {
      GameTestHelpers.assertTrue(
          helper,
          npcType.getDescriptionId() + " NPC must not burn in daylight",
          !invokeProtectedBoolean(npcEntity, "isSunBurnTick"));
    } else {
      helper.fail(npcType.getDescriptionId() + " is neither a zombie nor a skeleton");
    }
  }

  private static boolean invokeProtectedBoolean(Entity entity, String methodName) {
    for (Class<?> type = entity.getClass(); type != null; type = type.getSuperclass()) {
      try {
        Method method = type.getDeclaredMethod(methodName);
        method.setAccessible(true);
        return (boolean) method.invoke(entity);
      } catch (NoSuchMethodException e) {
        continue;
      } catch (ReflectiveOperationException e) {
        throw new IllegalStateException("Unable to call " + methodName + " on " + entity, e);
      }
    }
    throw new IllegalStateException(entity.getClass() + " has no method " + methodName);
  }
}
