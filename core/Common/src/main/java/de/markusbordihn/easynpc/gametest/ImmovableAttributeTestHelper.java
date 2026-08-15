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

import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;

public class ImmovableAttributeTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final double KNOCKBACK_STRENGTH = 2.0D;
  private static final float KNOCKBACK_DAMAGE = 1.0F;

  private ImmovableAttributeTestHelper() {}

  private static Vec3 knockbackMovement(GameTestHelper helper, EasyNPC<?> easyNPC) {
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    livingEntity.setDeltaMovement(Vec3.ZERO);
    livingEntity.knockback(
        KNOCKBACK_STRENGTH,
        1.0D,
        1.0D,
        helper.getLevel().damageSources().generic(),
        KNOCKBACK_DAMAGE,
        false);
    return livingEntity.getDeltaMovement();
  }

  public static void assertImmovableNPCIsNotPushed(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setInteractionAttribute(easyNPC, InteractionAttributeType.IS_PUSHABLE, true);

    GameTestHelpers.assertTrue(
        helper, "A pushable NPC must be pushable", easyNPC.getEntity().isPushable());
    GameTestHelpers.assertTrue(
        helper,
        "A pushable NPC must be moved by a knockback",
        !knockbackMovement(helper, easyNPC).equals(Vec3.ZERO));

    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.IS_IMMOVABLE, true);

    GameTestHelpers.assertTrue(
        helper, "An immovable NPC must not be pushable", !easyNPC.getEntity().isPushable());
    GameTestHelpers.assertEquals(
        helper,
        "An immovable NPC must not be moved by a knockback",
        Vec3.ZERO,
        knockbackMovement(helper, easyNPC));
  }

  public static void assertImmovableNPCLosesItsMovementObjectives(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setEnvironmentalAttribute(easyNPC, EnvironmentalAttributeType.CAN_FLOAT, true);
    easyNPC.getEasyNPCObjectiveData().registerAttributeBasedObjectives();

    GameTestHelpers.assertTrue(
        helper,
        "An NPC which can float must get the float objective",
        easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet().hasObjective(ObjectiveType.FLOAT));

    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.IS_IMMOVABLE, true);
    easyNPC.getEasyNPCObjectiveData().registerAttributeBasedObjectives();

    GameTestHelpers.assertTrue(
        helper,
        "An immovable NPC must lose the float objective",
        !easyNPC.getEasyNPCObjectiveData().getObjectiveDataSet().hasObjective(ObjectiveType.FLOAT));
  }
}
