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

import de.markusbordihn.easynpc.data.skin.variant.VillagerSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.npc.villager.Villager;
import net.minecraft.world.entity.npc.villager.VillagerData;
import net.minecraft.world.phys.Vec3;

public class VillagerVariantTestHelper {

  private VillagerVariantTestHelper() {}

  public static void assertVillagerVariantResolvesProfessionAndType(
      GameTestHelper helper, EntityType<?> villagerEntityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, villagerEntityType, new Vec3(2, 2, 1));
    VariantDataCapable<?> variantData = npc.getEasyNPCVariantData();
    GameTestHelpers.assertNotNull(helper, "Villager NPC must expose variant data", variantData);
    GameTestHelpers.assertTrue(
        helper,
        "Villager NPC entity must be a vanilla Villager",
        npc.getEntity() instanceof Villager);
    Villager villager = (Villager) npc.getEntity();

    assertMapping(
        helper, variantData, villager, VillagerSkinVariant.DESERT_ARMORER, "desert", "armorer");
    assertMapping(
        helper,
        variantData,
        villager,
        VillagerSkinVariant.TAIGA_WEAPONSMITH,
        "taiga",
        "weaponsmith");
    assertMapping(helper, variantData, villager, VillagerSkinVariant.PLAINS_NONE, "plains", "none");
  }

  private static void assertMapping(
      GameTestHelper helper,
      VariantDataCapable<?> variantData,
      Villager villager,
      VillagerSkinVariant variant,
      String expectedType,
      String expectedProfession) {
    variantData.setSkinVariantType(variant);
    VillagerData villagerData = villager.getVillagerData();
    Identifier typeKey = villagerData.type().unwrapKey().map(ResourceKey::identifier).orElse(null);
    Identifier professionKey =
        villagerData.profession().unwrapKey().map(ResourceKey::identifier).orElse(null);
    GameTestHelpers.assertEquals(
        helper,
        variant + " should map to villager type minecraft:" + expectedType + " but was " + typeKey,
        Identifier.withDefaultNamespace(expectedType),
        typeKey);
    GameTestHelpers.assertEquals(
        helper,
        variant
            + " should map to profession minecraft:"
            + expectedProfession
            + " but was "
            + professionKey,
        Identifier.withDefaultNamespace(expectedProfession),
        professionKey);
  }
}
