// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

// Simple SRP-6a JavaScript auth function for use with CouchDB

/**
 * Requires jsbn.js, jsbn2.js, prng4.js, rng.js, json2.js, sha1.js.
 */
function authSRP(req, username, password, callback) {
  // 256-bit key
  var nStr = "115B8B692E0E045692CF280B436735C77A5A9E8A9E7ED56C965F87DB5B2A2ECE3";
  var n = new BigInteger(nStr, 16);

  var g = new BigInteger("2");
  // Generate random number a, 1 < a < n
  var a = new BigInteger(nStr.length * 4, new SecureRandom()).mod(n);
  // Compute ephemeral public key A = g^a
  var A = g.modPow(a, n).toString(16).toUpperCase();

  req(JSON.stringify({username: username, A: A}), function(data) {
    if (data.error) return callback(data);
    // Abort if B == 0 (mod N)
    var B = new BigInteger(data.B, 16).mod(n);
    if (B.equals(BigInteger.ZERO)) {
      return callback({error: 'Invalid public key sent by server.'});
    }
    // Compute long-term private key x = H(s, P)
    var x = new BigInteger(hex_sha1(data.s + password), 16);
    // k = H(N, g), in SRP-6a
    var k = new BigInteger(hex_sha1(nStr + g.toString(16)), 16);
    // u = H(A, B)
    var u = new BigInteger(hex_sha1(A + data.B), 16);
    // Abort if u == 0
    if (u.equals(BigInteger.ZERO)) {
      return callback({error: 'Invalid scrambling parameter generated from information sent by server.'});
    }
    // Compute common exponential value S = (B - kg^x)^(a + ux)
    var S = B.subtract(k.multiply(g.modPow(x, n))).modPow(a.add(u.multiply(x)), n);
    // Compute cryptographically strong session key K = H(S)
    var K = hex_sha1(S.toString(16).toUpperCase());
    // Compute M[1] = H(A, B, K)
    var M1 = hex_sha1(A + data.B + K);
    req(JSON.stringify({username: username, M1: M1, A: A, b: data.b, B: data.B, timestamp: data.timestamp}), function(data) {
      if (data.error) return callback(data);
      // Verify server's M[2] = H(A, M[1], K)
      var M2 = hex_sha1(A + M1 + K);
      if (M2 != data.M2) {
        return callback({error: 'Invalid verification message sent by server.'});
      }
      return callback({ok: true});
    });
  });
}
