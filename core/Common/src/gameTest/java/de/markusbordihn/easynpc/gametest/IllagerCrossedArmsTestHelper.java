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
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.AbstractIllager;
import net.minecraft.world.phys.Vec3;

public class IllagerCrossedArmsTestHelper {

  private IllagerCrossedArmsTestHelper() {}

  public static void assertCrossedArmsVariantDrivesArmPose(
      GameTestHelper helper,
      EntityType<?> illagerEntityType,
      Enum<?> baseVariant,
      Enum<?> crossedVariant) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, illagerEntityType, new Vec3(2, 2, 1));
    VariantDataCapable<?> variantData = npc.getEasyNPCVariantData();
    GameTestHelpers.assertNotNull(helper, "Illager NPC must expose variant data", variantData);
    GameTestHelpers.assertTrue(
        helper,
        "Illager NPC entity must be an AbstractIllager",
        npc.getEntity() instanceof AbstractIllager);
    AbstractIllager illager = (AbstractIllager) npc.getEntity();

    variantData.setSkinVariantType(crossedVariant);
    GameTestHelpers.assertEquals(
        helper,
        crossedVariant + " must render with crossed arms",
        AbstractIllager.IllagerArmPose.CROSSED,
        illager.getArmPose());

    variantData.setSkinVariantType(baseVariant);
    GameTestHelpers.assertEquals(
        helper,
        baseVariant + " must render with lowered arms",
        AbstractIllager.IllagerArmPose.NEUTRAL,
        illager.getArmPose());
  }
}
