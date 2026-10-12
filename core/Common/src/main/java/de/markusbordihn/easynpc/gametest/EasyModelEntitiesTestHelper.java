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

import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import de.markusbordihn.easynpc.handler.RenderHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.ai.navigation.FlyingPathNavigation;
import net.minecraft.world.entity.ai.navigation.WaterBoundPathNavigation;
import net.minecraft.world.phys.Vec3;

public class EasyModelEntitiesTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final String FLOATING_PROFILE = "easy_model_entities_examples:entity/wisp_lantern";
  private static final String AQUATIC_PROFILE = "easy_model_entities_examples:entity/coral_drifter";

  private EasyModelEntitiesTestHelper() {}

  public static void assertIntegrationListsExampleProfiles(GameTestHelper helper) {
    GameTestHelpers.assertTrue(
        helper,
        "Easy Model Entities must be detected with a supported API version",
        CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED);
    GameTestHelpers.assertTrue(
        helper,
        "The integration must list the default example profile",
        IntegrationRegistry.getModels(EasyModelEntitiesManager.INTEGRATION_ID)
            .contains(EasyModelEntitiesManager.DEFAULT_PROFILE));
  }

  public static void assertDefaultProfileDefinesTheNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyModelNPC easyModelNPC = mockEasyModelNPC(helper, entityType);

    GameTestHelpers.assertEquals(
        helper,
        "A new NPC must use the default profile",
        ResourceLocation.parse(EasyModelEntitiesManager.DEFAULT_PROFILE),
        easyModelNPC.getEasyModelProfileId());
    assertDimensions(helper, easyModelNPC, 0.75F, 1.75F);
    GameTestHelpers.assertEquals(
        helper,
        "The default profile must walk on the ground",
        NavigationType.GROUND,
        easyModelNPC.getEasyNPCNavigationData().getNavigationType());
  }

  public static void assertFloatingProfileFlies(GameTestHelper helper, EntityType<?> entityType) {
    EasyModelNPC easyModelNPC = mockEasyModelNPC(helper, entityType);
    NavigationDataCapable<?> navigationData = useProfile(easyModelNPC, FLOATING_PROFILE);

    GameTestHelpers.assertEquals(
        helper,
        "A floating profile must default to the flying navigation",
        NavigationType.FLYING,
        navigationData.getNavigationType());
    GameTestHelpers.assertTrue(
        helper,
        "A floating profile must use the flying path navigation",
        easyModelNPC.getNavigation() instanceof FlyingPathNavigation);
    GameTestHelpers.assertEquals(
        helper,
        "A floating profile must default to the profile hover height",
        EasyModelEntitiesManager.DEFAULT_HOVER_HEIGHT,
        navigationData.getHoverHeight());
    assertDimensions(helper, easyModelNPC, 0.4F, 1.0F);
  }

  public static void assertAquaticProfileSwims(GameTestHelper helper, EntityType<?> entityType) {
    EasyModelNPC easyModelNPC = mockEasyModelNPC(helper, entityType);
    NavigationDataCapable<?> navigationData = useProfile(easyModelNPC, AQUATIC_PROFILE);

    GameTestHelpers.assertEquals(
        helper,
        "An aquatic profile must default to the aquatic navigation",
        NavigationType.AQUATIC,
        navigationData.getNavigationType());
    GameTestHelpers.assertTrue(
        helper,
        "An aquatic profile must use the water bound path navigation",
        easyModelNPC.getNavigation() instanceof WaterBoundPathNavigation);
  }

  public static void assertProfileSurvivesPresetImport(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyModelNPC easyModelNPC = mockEasyModelNPC(helper, entityType);
    useProfile(easyModelNPC, FLOATING_PROFILE);

    EasyModelNPC importedNPC = mockEasyModelNPC(helper, entityType);
    importedNPC
        .getEasyNPCPresetData()
        .importPresetData(easyModelNPC.getEasyNPCPresetData().serializePresetData());

    GameTestHelpers.assertEquals(
        helper,
        "An imported preset must keep the profile",
        ResourceLocation.parse(FLOATING_PROFILE),
        importedNPC.getEasyModelProfileId());
  }

  private static EasyModelNPC mockEasyModelNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    if (easyNPC instanceof EasyModelNPC easyModelNPC) {
      return easyModelNPC;
    }

    helper.fail("Entity " + entityType + " is not an Easy Model NPC!");
    return null;
  }

  private static NavigationDataCapable<?> useProfile(EasyModelNPC easyModelNPC, String profile) {
    RenderHandler.setRenderEntityModel(easyModelNPC, profile);
    NavigationDataCapable<?> navigationData = easyModelNPC.getEasyNPCNavigationData();
    navigationData.refreshNavigationIfChanged();
    return navigationData;
  }

  private static void assertDimensions(
      GameTestHelper helper, EasyModelNPC easyModelNPC, float width, float height) {
    EntityDimensions dimensions = easyModelNPC.getDimensions(Pose.STANDING);
    GameTestHelpers.assertEquals(
        helper, "The NPC width must follow the profile", width, dimensions.width());
    GameTestHelpers.assertEquals(
        helper, "The NPC height must follow the profile", height, dimensions.height());
  }
}
