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

package de.markusbordihn.easynpc.client.texture;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TextureModelKey Tests")
class TextureModelKeyTest {

  @Test
  @DisplayName("Should create TextureModelKey with all parameters")
  void testCreateWithAllParameters() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    String resourceName = "test_resource";
    TextureModelKey key = new TextureModelKey(uuid, skinModel, resourceName);

    assertEquals(uuid, key.uuid());
    assertEquals(skinModel, key.skinModel());
    assertEquals(resourceName, key.resourceName());
    assertEquals(skinModel.name(), key.getSubType());
  }

  @Test
  @DisplayName("Should create TextureModelKey without resourceName")
  void testCreateWithoutResourceName() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID_SLIM;
    TextureModelKey key = new TextureModelKey(uuid, skinModel);

    assertEquals(uuid, key.uuid());
    assertEquals(skinModel, key.skinModel());
    assertEquals("", key.resourceName());
    assertEquals(skinModel.name(), key.getSubType());
  }

  @Test
  @DisplayName("Should handle null resourceName")
  void testNullResourceName() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    TextureModelKey key = new TextureModelKey(uuid, skinModel, null);

    assertEquals("", key.getResourceName());
  }

  @Test
  @DisplayName("Should handle null skinModel in getSubType")
  void testNullSkinModel() {
    UUID uuid = UUID.randomUUID();
    TextureModelKey key = new TextureModelKey(uuid, null, "test");

    assertEquals("", key.getSubType());
    assertNull(key.skinModel());
  }

  @Test
  @DisplayName("Should use custom equals - resourceName is ignored")
  void testCustomEqualsIgnoresResourceName() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    TextureModelKey key1 = new TextureModelKey(uuid, skinModel, "resource1");
    TextureModelKey key2 = new TextureModelKey(uuid, skinModel, "resource2");
    TextureModelKey key3 = new TextureModelKey(uuid, skinModel, "");

    assertEquals(key1, key2);
    assertEquals(key1, key3);
    assertEquals(key2, key3);
  }

  @Test
  @DisplayName("Should use custom equals - uuid and subType must match")
  void testCustomEqualsRequiresUuidAndSubType() {
    UUID uuid1 = UUID.randomUUID();
    UUID uuid2 = UUID.randomUUID();
    TextureModelKey key1 = new TextureModelKey(uuid1, SkinModel.HUMANOID);
    TextureModelKey key2 = new TextureModelKey(uuid2, SkinModel.HUMANOID);
    TextureModelKey key3 = new TextureModelKey(uuid1, SkinModel.HUMANOID_SLIM);

    assertNotEquals(key1, key2);
    assertNotEquals(key1, key3);
  }

  @Test
  @DisplayName("Should use custom hashCode - resourceName is ignored")
  void testCustomHashCodeIgnoresResourceName() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    TextureModelKey key1 = new TextureModelKey(uuid, skinModel, "resource1");
    TextureModelKey key2 = new TextureModelKey(uuid, skinModel, "resource2");
    TextureModelKey key3 = new TextureModelKey(uuid, skinModel, "");

    assertEquals(key1.hashCode(), key2.hashCode());
    assertEquals(key1.hashCode(), key3.hashCode());
    assertEquals(key2.hashCode(), key3.hashCode());
  }

  @Test
  @DisplayName("Should use custom hashCode - different for different uuid/subType")
  void testCustomHashCodeDifferent() {
    UUID uuid1 = UUID.randomUUID();
    UUID uuid2 = UUID.randomUUID();
    TextureModelKey key1 = new TextureModelKey(uuid1, SkinModel.HUMANOID);
    TextureModelKey key2 = new TextureModelKey(uuid2, SkinModel.HUMANOID);
    TextureModelKey key3 = new TextureModelKey(uuid1, SkinModel.HUMANOID_SLIM);

    assertNotEquals(key1.hashCode(), key2.hashCode());
    assertNotEquals(key1.hashCode(), key3.hashCode());
  }

  @Test
  @DisplayName("Should work correctly in HashMap due to custom equals/hashCode")
  void testHashMapBehavior() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;

    TextureModelKey key1 = new TextureModelKey(uuid, skinModel, "resource1");
    TextureModelKey key2 = new TextureModelKey(uuid, skinModel, "resource2");

    Set<TextureModelKey> set = new HashSet<>();
    set.add(key1);

    assertTrue(set.contains(key2));
    assertFalse(set.add(key2));

    assertEquals(1, set.size());
  }

  @Test
  @DisplayName("Should maintain reflexivity of equals")
  void testEqualsReflexivity() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);

    assertEquals(key, key);
  }

  @Test
  @DisplayName("Should maintain symmetry of equals")
  void testEqualsSymmetry() {
    UUID uuid = UUID.randomUUID();
    TextureModelKey key1 = new TextureModelKey(uuid, SkinModel.HUMANOID);
    TextureModelKey key2 = new TextureModelKey(uuid, SkinModel.HUMANOID);

    assertEquals(key1, key2);
    assertEquals(key2, key1);
  }

  @Test
  @DisplayName("Should maintain transitivity of equals")
  void testEqualsTransitivity() {
    UUID uuid = UUID.randomUUID();
    TextureModelKey key1 = new TextureModelKey(uuid, SkinModel.HUMANOID);
    TextureModelKey key2 = new TextureModelKey(uuid, SkinModel.HUMANOID);
    TextureModelKey key3 = new TextureModelKey(uuid, SkinModel.HUMANOID);

    assertEquals(key1, key2);
    assertEquals(key2, key3);
    assertEquals(key1, key3);
  }

  @Test
  @DisplayName("Should return false when comparing to null")
  void testEqualsWithNull() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);

    assertNotEquals(null, key);
  }

  @Test
  @DisplayName("Should return false when comparing to different type")
  void testEqualsWithDifferentType() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID);

    assertNotEquals(key, "not a TextureModelKey");
    assertNotEquals(key, UUID.randomUUID());
  }

  @Test
  @DisplayName("Should have consistent toString output")
  void testToString() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    String resourceName = "test_resource";
    TextureModelKey key = new TextureModelKey(uuid, skinModel, resourceName);
    String toString = key.toString();

    assertTrue(toString.contains("TextureModelKey"));
    assertTrue(toString.contains(uuid.toString()));
    assertTrue(toString.contains(skinModel.toString()));
    assertTrue(toString.contains(resourceName));
  }

  @Test
  @DisplayName("Should test all SkinModel types")
  void testAllSkinModelTypes() {
    UUID uuid = UUID.randomUUID();

    for (SkinModel skinModel : SkinModel.values()) {
      TextureModelKey key = new TextureModelKey(uuid, skinModel);
      assertEquals(uuid, key.uuid());
      assertEquals(skinModel, key.skinModel());
      assertEquals(skinModel.name(), key.getSubType());
    }
  }

  @Test
  @DisplayName("Should be immutable - record properties")
  void testImmutability() {
    UUID uuid = UUID.randomUUID();
    SkinModel skinModel = SkinModel.HUMANOID;
    String resourceName = "test";
    TextureModelKey key = new TextureModelKey(uuid, skinModel, resourceName);

    // Record fields should be accessible but not modifiable
    assertEquals(uuid, key.uuid());
    assertEquals(skinModel, key.skinModel());
    assertEquals(resourceName, key.resourceName());

    // Trying to create a new instance should create a different object
    TextureModelKey key2 = new TextureModelKey(uuid, skinModel, "different");
    assertNotSame(key, key2);
    assertEquals(key, key2);
  }
}
