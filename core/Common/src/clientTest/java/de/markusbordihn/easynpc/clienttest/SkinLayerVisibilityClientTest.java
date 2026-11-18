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
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.clienttest;

import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SkinLayerVisibilityClientTest extends ClientTestBase {

  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final long SETTLED_TICKS = 5;
  private static final long HIDDEN_LAYER_NPC_RENDER_TICKS = 20;
  private static final long PLATFORM_LIGHTING_TICKS = 40;
  private static final String FOREIGN_NPC_TYPE = "easy_npc:zombie";
  private static final String LAYER_TEMPLATE_SKIN_DATA =
      "SkinData:{Type:\"RESOURCE_LOCATION\","
          + "Texture:\"easy_npc:textures/entity/humanoid/humanoid_template.png\"}";
  private static final String HIDDEN_OUTER_LAYERS_MODEL_DATA =
      "ModelData:{Pose:\"CUSTOM\",Visible:{Hat:0b,BodyJacket:0b,LeftSleeve:0b,RightSleeve:0b,"
          + "LeftPants:0b,RightPants:0b}}";
  private static final String HIDDEN_INNER_PARTS_MODEL_DATA =
      "AnimationData:{Behavior:\"NONE\"},"
          + "ModelData:{Pose:\"CUSTOM\",Visible:{Head:0b,Body:0b,LeftArm:0b}}";
  private static final String HELD_ITEMS_DATA =
      "equipment:{mainhand:{id:\"minecraft:diamond_sword\",count:1},"
          + "offhand:{id:\"minecraft:shield\",count:1}}";
  private static final String LIBRARIAN_VARIANT_DATA = "VariantType:\"PLAINS_LIBRARIAN\"";
  private static final Set<String> PROFESSION_LAYER_TYPES = Set.of("villager", "zombie_villager");
  private static final String FROZEN_TRANSFORMED_POSE_DATA =
      "AnimationData:{Behavior:\"NONE\"},ModelData:{Pose:\"CUSTOM\","
          + "Rotation:{Head:[0.6f,0.4f,0.0f],Body:[0.3f,0.0f,0.0f],RightArm:[-1.4f,0.0f,0.0f],"
          + "LeftArm:[-1.4f,0.0f,0.0f],RightLeg:[0.5f,0.0f,0.0f],LeftLeg:[-0.5f,0.0f,0.0f]},"
          + "Position:{Head:[0.0f,-2.0f,0.0f],RightArm:[-1.0f,0.0f,0.0f]},"
          + "Scale:{Head:[1.5f,1.5f,1.5f],Body:[1.2f,1.2f,1.2f],LeftArm:[0.5f,0.5f,0.5f]}}";
  private static final String HIDDEN_HEAD_MODEL_DATA =
      "ModelData:{Pose:\"CUSTOM\",Visible:{Head:0b}}";
  private static final String HIDDEN_HAT_MODEL_DATA =
      "ModelData:{Pose:\"CUSTOM\",Visible:{Hat:0b}}";
  private static final String HIDDEN_NAME_TAG_DATA =
      "DisplayAttribute:[{Type:\"NAME_VISIBILITY\",Text:\"NEVER\"}]";
  private static final double MAXIMUM_HEAD_CHANGED_RATIO = 0.08;
  private static final double MAXIMUM_BODY_CHANGED_RATIO = 0.08;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(32);

  private static String retainEmptyPlatformFrame() {
    awaitWorldSetupCapability();
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.await(Until.ticksElapsed(PLATFORM_LIGHTING_TICKS));
    String emptyPlatformFrame = client.retainFrame();
    return emptyPlatformFrame;
  }

  private static List<String> changedNPCsInSharedScene(String label, List<SceneNPC> sceneNPCs) {
    String emptyPlatformFrame = retainEmptyPlatformFrame();

    Map<SceneNPC, CaptureCrop> soloBounds = new LinkedHashMap<>();
    Map<SceneNPC, String> soloFrames = new LinkedHashMap<>();
    List<SceneNPC> unchangedObserversFirst =
        sceneNPCs.stream().sorted(Comparator.comparing(SceneNPC::role)).toList();
    for (SceneNPC sceneNPC : unchangedObserversFirst) {
      UUID soloNPCUUID = spawnSceneNPC(sceneNPC);
      soloBounds.put(sceneNPC, changedBounds(emptyPlatformFrame));
      soloFrames.put(sceneNPC, client.retainFrame());
      removeAndAwaitAbsent(soloNPCUUID);
    }

    List<UUID> sceneNPCUUIDs = new ArrayList<>();
    for (SceneNPC sceneNPC : sceneNPCs) {
      sceneNPCUUIDs.add(spawnSceneNPC(sceneNPC));
    }
    client.await(Until.ticksElapsed(HIDDEN_LAYER_NPC_RENDER_TICKS));
    client.saveScreenshot(label + "_shared_scene");

    List<String> changedNPCs = new ArrayList<>();
    for (SceneNPC sceneNPC : sceneNPCs) {
      String soloFrame = soloFrames.get(sceneNPC);
      CaptureCrop npcBounds = soloBounds.get(sceneNPC);
      double headChange = centerColumnChange(soloFrame, npcBounds, npcBounds.height() / 4);
      double bodyChange = centerColumnChange(soloFrame, npcBounds, npcBounds.height());
      if (headChange > MAXIMUM_HEAD_CHANGED_RATIO || bodyChange > MAXIMUM_BODY_CHANGED_RATIO) {
        changedNPCs.add(
            sceneNPC.name() + " head " + headChange + " body " + bodyChange + " " + npcBounds);
      }
    }
    sceneNPCUUIDs.forEach(SkinLayerVisibilityClientTest::removeAndAwaitAbsent);
    return changedNPCs;
  }

  private static String joinNPCData(String... npcDataParts) {
    return String.join(
        ",", Arrays.stream(npcDataParts).filter(npcDataPart -> !npcDataPart.isEmpty()).toList());
  }

  private static UUID spawnFacingPlayer(EntitySpawn entitySpawn, String npcData) {
    UUID npcUUID =
        client.spawnEntity(
            entitySpawn.withRotation(180, 0).withNoAi(true).withNbt("{" + npcData + "}"));
    client.await(
        Until.entityPresent(
            Parameters.of("type", entitySpawn.type()).with("uuid", npcUUID.toString())));
    return npcUUID;
  }

  private static UUID spawnWithoutNameFacingPlayer(EntitySpawn entitySpawn, String npcData) {
    UUID npcUUID = spawnFacingPlayer(entitySpawn, joinNPCData(HIDDEN_NAME_TAG_DATA, npcData));
    client.await(Until.settled(SETTLED_TICKS));
    return npcUUID;
  }

  private static UUID spawnSceneNPC(SceneNPC sceneNPC) {
    return spawnWithoutNameFacingPlayer(
        EntitySpawn.of(sceneNPC.entityId(), sceneNPC.x(), VIEW_Y, sceneNPC.z()),
        sceneNPC.npcData());
  }

  private static CaptureCrop changedBounds(String referenceFrame) {
    return client.compare(referenceFrame, MODEL_COMPARE).comparison().changedBounds().orElseThrow();
  }

  private static double centerColumnChange(
      String referenceFrame, CaptureCrop npcBounds, long columnHeight) {
    long columnWidth = Math.max(1, npcBounds.width() / 3);
    return client
        .compare(
            referenceFrame,
            MODEL_COMPARE.withRegion(
                (int) (npcBounds.x() + (npcBounds.width() - columnWidth) / 2),
                (int) npcBounds.y(),
                (int) columnWidth,
                (int) Math.max(1, columnHeight)))
        .comparison()
        .changedRatio();
  }

  private static void removeAndAwaitAbsent(UUID npcUUID) {
    client.removeEntity(npcUUID);
    client.await(Until.entityAbsent(Parameters.of("uuid", npcUUID.toString())));
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"humanoid", "humanoid_slim"})
  @DisplayName("NPCs with different model settings keep their own look in one scene")
  void npcsKeepTheirOwnLookInSharedScene(String npcType) {
    String npcId = "easy_npc:" + npcType;
    List<SceneNPC> sceneNPCs =
        List.of(
            SceneNPC.observer("center_observer", npcId, 0.5, 3.5, LAYER_TEMPLATE_SKIN_DATA),
            SceneNPC.changed(
                "back_left_hidden_outer_layers",
                npcId,
                3.5,
                7.5,
                joinNPCData(LAYER_TEMPLATE_SKIN_DATA, HIDDEN_OUTER_LAYERS_MODEL_DATA)),
            SceneNPC.changed(
                "front_left_frozen_pose_with_items",
                npcId,
                2.5,
                3.0,
                joinNPCData(
                    LAYER_TEMPLATE_SKIN_DATA, FROZEN_TRANSFORMED_POSE_DATA, HELD_ITEMS_DATA)),
            SceneNPC.changed(
                "front_right_hidden_inner_parts",
                npcId,
                -1.5,
                3.0,
                joinNPCData(LAYER_TEMPLATE_SKIN_DATA, HIDDEN_INNER_PARTS_MODEL_DATA)),
            SceneNPC.changed(
                "far_back_foreign_type", FOREIGN_NPC_TYPE, 3.5, 11.5, HIDDEN_HAT_MODEL_DATA),
            SceneNPC.observer(
                "back_right_late_observer", npcId, -2.5, 7.5, LAYER_TEMPLATE_SKIN_DATA));

    List<String> changedNPCs = changedNPCsInSharedScene(npcType, sceneNPCs);

    assertTrue(
        changedNPCs.isEmpty(), npcType + " NPCs changed in the shared scene: " + changedNPCs);
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(
      strings = {
        "bogged",
        "doppler",
        "drowned",
        "evoker",
        "husk",
        "illusioner",
        "orc",
        "orc_warrior",
        "piglin",
        "piglin_brute",
        "pillager",
        "skeleton",
        "stray",
        "villager",
        "vindicator",
        "wandering_trader",
        "witch",
        "wither_skeleton",
        "zombie",
        "zombie_villager",
        "zombified_piglin"
      })
  @DisplayName("NPCs of one type keep their own layers, pose and items next to each other")
  void sameTypeNPCsKeepTheirOwnLayersPoseAndItems(String entityType) {
    String entityId = "easy_npc:" + entityType;
    String variantData = PROFESSION_LAYER_TYPES.contains(entityType) ? LIBRARIAN_VARIANT_DATA : "";
    List<SceneNPC> sceneNPCs =
        List.of(
            SceneNPC.observer("center_vanilla_pose", entityId, 0.5, 3.5, variantData),
            SceneNPC.changed(
                "back_right_frozen_pose_with_items",
                entityId,
                -2.5,
                7.5,
                joinNPCData(variantData, FROZEN_TRANSFORMED_POSE_DATA, HELD_ITEMS_DATA)),
            SceneNPC.changed(
                "back_left_hidden_outer_layers",
                entityId,
                3.5,
                7.5,
                joinNPCData(variantData, HIDDEN_OUTER_LAYERS_MODEL_DATA)),
            SceneNPC.observer("far_right_late_vanilla_pose", entityId, -6.5, 9.5, variantData));

    List<String> changedNPCs = changedNPCsInSharedScene(entityType, sceneNPCs);

    assertTrue(
        changedNPCs.isEmpty(), entityType + " NPCs changed next to each other: " + changedNPCs);
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"villager", "zombie_villager"})
  @DisplayName("Hidden head stays hidden under the profession overlay")
  void hiddenHeadStaysHiddenUnderProfessionOverlay(String entityType) {
    EntitySpawn npcSpawn = EntitySpawn.of("easy_npc:" + entityType, 0.5, VIEW_Y, 3.5);
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID visibleHeadNPCUUID = spawnWithoutNameFacingPlayer(npcSpawn, LIBRARIAN_VARIANT_DATA);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    removeAndAwaitAbsent(visibleHeadNPCUUID);

    UUID hiddenHeadNPCUUID =
        spawnWithoutNameFacingPlayer(
            npcSpawn, joinNPCData(LIBRARIAN_VARIANT_DATA, HIDDEN_HEAD_MODEL_DATA));
    client.await(Until.ticksElapsed(HIDDEN_LAYER_NPC_RENDER_TICKS));
    double headChange = centerColumnChange(emptyPlatformFrame, npcBounds, npcBounds.height() / 4);
    client.saveScreenshot(entityType + "_hidden_head");
    removeAndAwaitAbsent(hiddenHeadNPCUUID);

    assertTrue(
        headChange <= MAXIMUM_HEAD_CHANGED_RATIO,
        entityType + " hidden head still renders " + headChange + " of its head area");
  }

  private enum SceneRole {
    UNCHANGED_OBSERVER,
    CHANGED
  }

  private record SceneNPC(
      String name, String entityId, double x, double z, String npcData, SceneRole role) {

    private static SceneNPC observer(
        String name, String entityId, double x, double z, String npcData) {
      return new SceneNPC(name, entityId, x, z, npcData, SceneRole.UNCHANGED_OBSERVER);
    }

    private static SceneNPC changed(
        String name, String entityId, double x, double z, String npcData) {
      return new SceneNPC(name, entityId, x, z, npcData, SceneRole.CHANGED);
    }
  }
}
